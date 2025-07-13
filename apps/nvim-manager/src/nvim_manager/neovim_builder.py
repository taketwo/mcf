"""Neovim build system."""

import subprocess
import tempfile
from pathlib import Path
from typing import TextIO

from git import Repo

from .logging import get_logger
from .utils import run_command

logger = get_logger(__name__)


class NeovimBuilder:
    """Builds Neovim from source code at specified Git commits.

    This class manages the complete build process for Neovim, including
    dependency compilation, source code management, and installation.
    It implements Neovim's official two-stage build process using CMake
    and Ninja, with build isolation and caching for efficiency.

    The builder maintains a persistent source repository clone and creates
    isolated temporary build directories for each build operation. Built
    executables are cached by commit hash to avoid redundant builds.

    Attributes
    ----------
    build_cache : Path
        Root directory for caching source code and built installations.
    source_dir : Path
        Directory containing the shared Neovim source repository.

    """

    def __init__(self, build_cache: Path) -> None:
        """Initialize Neovim builder with cache directory.

        Parameters
        ----------
        build_cache : Path
            Root directory for caching source code and installations.

        """
        self.build_cache = build_cache
        self.source_dir = build_cache / "neovim"

    def build_commit(self, commit_hash: str) -> Path:
        """Build Neovim from a specific commit hash.

        Parameters
        ----------
        commit_hash : str
            Git commit hash to build.

        Returns
        -------
        Path
            Path to the built nvim executable in build cache.

        Raises
        ------
        git.exc.GitCommandError
            If git operations fail.
        subprocess.CalledProcessError
            If build process fails.
        RuntimeError
            If required build tools are not available.

        """
        short_hash = commit_hash[:8]
        logger.debug("Building Neovim from commit %s", short_hash)

        # Install to cache directory named after commit hash
        install_cache_dir = self.build_cache / "installs" / short_hash
        nvim_executable = install_cache_dir / "bin" / "nvim"

        # Check if already built
        if nvim_executable.exists():
            logger.debug(
                "Found cached Neovim binary for commit %s at %s",
                short_hash,
                nvim_executable,
            )
            return nvim_executable

        self._ensure_source_at_commit(commit_hash)

        with tempfile.TemporaryDirectory(
            prefix=f"nvim-build-{short_hash}-",
            delete=False,
        ) as isolated_build_dir:
            build_success = self._build_and_install(
                Path(isolated_build_dir),
                install_cache_dir,
            )

        if not build_success:
            msg = f"Build failed for commit {short_hash}"
            logger.error(msg)
            raise RuntimeError(msg)

        if not nvim_executable.exists():
            msg = f"Build completed but executable not found at {nvim_executable}"
            logger.error(msg)
            raise RuntimeError(msg)

        logger.debug(
            "Successfully built Neovim for commit %s: %s",
            short_hash,
            nvim_executable,
        )
        return nvim_executable

    def _ensure_source_at_commit(self, commit_hash: str) -> None:
        """Ensure source code is available and checked out to specific commit."""
        short_hash = commit_hash[:8]

        if not self.source_dir.exists():
            logger.info(
                "Cloning Neovim repository to %s",
                self.source_dir,
            )
            self.build_cache.mkdir(parents=True, exist_ok=True)
            Repo.clone_from("https://github.com/neovim/neovim.git", self.source_dir)
        else:
            logger.debug("Neovim repository already exists at %s", self.source_dir)

        repo = Repo(self.source_dir)
        logger.debug("Fetching latest changes from remote")
        repo.remotes.origin.fetch()

        logger.debug("Checking out commit %s", short_hash)
        repo.git.checkout(commit_hash)

    def _run_command_with_logging(
        self,
        args: list[str],
        cwd: Path | None,
        stdout_file: TextIO,
        stderr_file: TextIO,
    ) -> bool:
        """Run command and log output to files.

        Parameters
        ----------
        args : list[str]
            Command and arguments to execute.
        cwd : Path | None
            Working directory for command execution.
        stdout_file : TextIO
            File handle to write stdout output to.
        stderr_file : TextIO
            File handle to write stderr output to.

        Returns
        -------
        bool
            True if command succeeded, False otherwise.

        """
        command_str = " ".join(args)

        # Write command header to both files
        stdout_file.write(f"\n=== Running: {command_str} ===\n")
        stderr_file.write(f"\n=== Running: {command_str} ===\n")

        def write_output(stdout: str | None, stderr: str | None) -> None:
            """Write output to files and flush."""
            if stdout:
                stdout_file.write(stdout)
                stdout_file.write("\n")
            if stderr:
                stderr_file.write(stderr)
                stderr_file.write("\n")
            stdout_file.flush()
            stderr_file.flush()

        try:
            result = run_command(args, cwd=cwd, capture_output=True)
        except subprocess.CalledProcessError as e:
            # Write error information for command failures
            stderr_file.write(f"Command failed with exit code {e.returncode}: {e}\n")
            write_output(e.stdout, e.stderr)
            return False
        else:
            write_output(result.stdout, result.stderr)
            return True

    def _build_and_install(self, build_path: Path, install_dir: Path) -> bool:
        """Configure, build, and install Neovim using the two-stage process.

        Parameters
        ----------
        build_path : Path
            Temporary directory for building.
        install_dir : Path
            Directory to install the built executable.

        Returns
        -------
        bool
            True if build succeeded, False otherwise.

        """
        logger.debug("Starting build and install process")
        logger.debug("Build directory: %s", build_path)
        logger.debug("Install directory: %s", install_dir)

        deps_dir = build_path / "deps"
        nvim_build_dir = build_path / "build"

        commands = [
            # Stage 1: Build dependencies
            [
                "cmake",
                "-S",
                "cmake.deps",
                "-B",
                str(deps_dir),
                "-G",
                "Ninja",
                "-D",
                "CMAKE_BUILD_TYPE=Release",
            ],
            ["cmake", "--build", str(deps_dir)],
            # Stage 2: Build Neovim
            [
                "cmake",
                "-B",
                str(nvim_build_dir),
                "-G",
                "Ninja",
                "-D",
                "CMAKE_BUILD_TYPE=Release",
                f"-DDEPS_PREFIX={deps_dir}/usr",
                f"-DCMAKE_INSTALL_PREFIX={install_dir}",
            ],
            ["cmake", "--build", str(nvim_build_dir)],
            # Install Neovim
            ["cmake", "--install", str(nvim_build_dir)],
        ]

        logger.debug("Executing %d build commands", len(commands))
        logger.debug("Stdout and stderr will be logged to files in %s", build_path)

        with (
            (build_path / "stdout.txt").open("w") as stdout_f,
            (build_path / "stderr.txt").open("w") as stderr_f,
        ):
            for i, command in enumerate(commands, 1):
                logger.debug(
                    "Executing command %d/%d: %s",
                    i,
                    len(commands),
                    " ".join(command),
                )
                if not self._run_command_with_logging(
                    command,
                    self.source_dir,
                    stdout_f,
                    stderr_f,
                ):
                    logger.error(
                        "Build command failed at step %d/%d; check logs in %s",
                        i,
                        len(commands),
                        build_path,
                    )
                    return False
        return True

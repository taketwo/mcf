"""Neovim build system."""

import shutil
import subprocess
import tempfile
import zipfile
from pathlib import Path
from typing import TextIO

import requests

from .logging import get_logger
from .utils import run_command

logger = get_logger(__name__)


class NeovimBuilder:
    """Builds Neovim from source code at specified Git commits.

    This class manages the complete build process for Neovim by downloading
    source code from GitHub API as ZIP files and using Neovim's official
    two-stage build process with CMake and Ninja.

    Only built executables are cached permanently. Source code and build
    artifacts are stored in temporary directories and cleaned up after
    each build to minimize disk usage.

    Attributes
    ----------
    build_cache : Path
        Root directory for caching built installations only.

    """

    def __init__(self, build_cache: Path) -> None:
        """Initialize Neovim builder with cache directory.

        Parameters
        ----------
        build_cache : Path
            Root directory for caching built installations.

        """
        self.build_cache = build_cache

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
        requests.HTTPError
            If GitHub API requests fail.
        subprocess.CalledProcessError
            If build process fails.
        RuntimeError
            If required build tools are not available.

        """
        short_hash = commit_hash[:8]
        logger.debug("Building Neovim from commit %s", short_hash)

        # Install to cache directory named after commit hash
        install_cache_dir = self.build_cache / short_hash
        nvim_executable = install_cache_dir / "bin" / "nvim"

        # Check if already built
        if nvim_executable.exists():
            logger.debug(
                "Found cached Neovim binary for commit %s at %s",
                short_hash,
                nvim_executable,
            )
            return nvim_executable

        # Download, build, and install in temporary directories
        self._download_and_build(commit_hash, install_cache_dir)

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

    def _download_and_build(self, commit_hash: str, install_dir: Path) -> None:
        """Download source and build Neovim in temporary directories.

        Parameters
        ----------
        commit_hash : str
            Git commit hash to build.
        install_dir : Path
            Directory to install the built executable.

        Raises
        ------
        requests.HTTPError
            If GitHub API request fails.
        subprocess.CalledProcessError
            If build process fails.
        OSError
            If filesystem operations fail.

        """
        short_hash = commit_hash[:8]

        # Create temporary directories for source and build
        with tempfile.TemporaryDirectory(
            prefix=f"nvim-source-{short_hash}-",
        ) as temp_source_str:
            source_dir = Path(temp_source_str)
            logger.debug("Using temporary source directory: %s", source_dir)

            # Download and extract source code
            self._download_source_zip(commit_hash, source_dir)

            # Build in temporary directory
            with tempfile.TemporaryDirectory(
                prefix=f"nvim-build-{short_hash}-",
            ) as temp_build_str:
                build_dir = Path(temp_build_str)
                logger.debug("Using temporary build directory: %s", build_dir)

                success = self._build_and_install(
                    source_dir,
                    build_dir,
                    install_dir,
                    short_hash,
                )

                if not success:
                    msg = f"Build failed for commit {short_hash}"
                    logger.error(msg)
                    raise RuntimeError(msg)

    def _download_source_zip(self, commit_hash: str, source_dir: Path) -> None:
        """Download and extract source code from GitHub API.

        Parameters
        ----------
        commit_hash : str
            Git commit hash to download.
        source_dir : Path
            Temporary directory to extract source code to.

        Raises
        ------
        requests.HTTPError
            If GitHub API request fails.
        OSError
            If filesystem operations fail.

        """
        short_hash = commit_hash[:8]

        try:
            # Download ZIP from GitHub API
            zip_url = (
                f"https://api.github.com/repos/neovim/neovim/zipball/{commit_hash}"
            )
            logger.debug("Downloading ZIP from: %s", zip_url)

            response = requests.get(zip_url, timeout=30)
            response.raise_for_status()

            # Log download size
            download_size_mb = len(response.content) / (1024 * 1024)
            logger.debug(
                "Downloaded ZIP for commit %s: %.1f MB",
                short_hash,
                download_size_mb,
            )

            # Extract ZIP to temporary file then to directory
            with tempfile.NamedTemporaryFile(suffix=".zip", delete=False) as zip_file:
                zip_file.write(response.content)
                zip_file_path = Path(zip_file.name)

            try:
                with zipfile.ZipFile(zip_file_path, "r") as zip_ref:
                    # GitHub zips have a top-level directory, extract and flatten
                    zip_ref.extractall(source_dir.parent)

                    # Find the extracted directory (GitHub creates neovim-{commit} format)
                    extracted_dirs = [
                        d
                        for d in source_dir.parent.iterdir()
                        if d.is_dir() and d.name.startswith("neovim-")
                    ]

                    if not extracted_dirs:
                        msg = "No extracted directory found"
                        raise RuntimeError(msg)

                    extracted_dir = extracted_dirs[0]

                    # Move contents to the expected source directory
                    if source_dir.exists():
                        shutil.rmtree(source_dir)
                    extracted_dir.rename(source_dir)

            finally:
                # Clean up temporary ZIP file
                zip_file_path.unlink(missing_ok=True)

            logger.debug("Source extraction completed for commit %s", short_hash)

        except requests.RequestException:
            logger.exception("Failed to download source ZIP for commit %s", short_hash)
            raise
        except (OSError, zipfile.BadZipFile):
            logger.exception("Failed to extract source ZIP for commit %s", short_hash)
            raise

    def _build_and_install(
        self,
        source_dir: Path,
        build_dir: Path,
        install_dir: Path,
        short_hash: str,
    ) -> bool:
        """Configure, build, and install Neovim using the two-stage process.

        Parameters
        ----------
        source_dir : Path
            Directory containing Neovim source code.
        build_dir : Path
            Temporary directory for building.
        install_dir : Path
            Directory to install the built executable.
        short_hash : str
            Short commit hash for logging.

        Returns
        -------
        bool
            True if build succeeded, False otherwise.

        """
        logger.debug("Starting build and install process for commit %s", short_hash)
        logger.debug("Source directory: %s", source_dir)
        logger.debug("Build directory: %s", build_dir)
        logger.debug("Install directory: %s", install_dir)

        deps_dir = build_dir / "deps"
        nvim_build_dir = build_dir / "build"

        commands = [
            # Stage 1: Build dependencies
            [
                "cmake",
                "-S",
                str(source_dir / "cmake.deps"),
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
                "-S",
                str(source_dir),
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
        logger.debug("Stdout and stderr will be logged to files in %s", build_dir)

        with (
            (build_dir / "stdout.txt").open("w") as stdout_f,
            (build_dir / "stderr.txt").open("w") as stderr_f,
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
                    None,  # Use current working directory
                    stdout_f,
                    stderr_f,
                ):
                    logger.error(
                        "Build command failed at step %d/%d; check logs in %s",
                        i,
                        len(commands),
                        build_dir,
                    )
                    return False
        return True

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

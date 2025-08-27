"""Neovim build system."""

import shutil
import subprocess
import tempfile
from pathlib import Path
from typing import TextIO

from git import Repo

from .logging import get_logger
from .utils import run_command

logger = get_logger(__name__)


class NeovimBuildError(Exception):
    """Base exception for Neovim build failures."""


class NeovimCloneError(NeovimBuildError):
    """Exception raised when git clone operations fail."""


class NeovimCompileError(NeovimBuildError):
    """Exception raised when compilation fails."""


class NeovimBuilder:
    """Builds Neovim from source code at specified Git commit.

    This class manages the complete build process for Neovim using optimized shallow
    clones and caching. Build artifacts and logs are preserved for debugging.

    Attributes
    ----------
    build_cache : Path
        Root directory for caching built installations.
    repository : str
        GitHub repository in format 'owner/repo' (e.g., 'neovim/neovim').

    """

    def __init__(
        self,
        build_cache: Path,
        repository: str,
        build_cache_size_limit: int,
    ) -> None:
        """Initialize Neovim builder with cache directory and repository.

        Parameters
        ----------
        build_cache : Path
            Root directory for caching built Neovim installations.
        repository : str
            GitHub repository in format 'owner/repo' (e.g., 'neovim/neovim').
        build_cache_size_limit : int
            Maximum number of cached builds to retain. When exceeded, oldest
            builds are automatically pruned.

        """
        self.build_cache = build_cache
        self.build_cache.mkdir(parents=True, exist_ok=True)
        self.repository = repository
        self.build_cache_size_limit = build_cache_size_limit

    def build_commit(self, commit_hash: str) -> Path:
        """Build Neovim from a specific commit hash.

        Creates an optimized shallow clone of the specific commit, builds Neovim using
        the standard two-stage CMake process, and installs to the build cache.

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
        NeovimCloneError
            If git operations fail.
        NeovimCompileError
            If build process fails or executable not found.

        """
        short_hash = commit_hash[:8]
        logger.debug("Building Neovim from commit %s", short_hash)

        # Check cache first
        install_cache_dir = self.build_cache / short_hash
        nvim_executable = install_cache_dir / "bin" / "nvim"
        if nvim_executable.exists():
            logger.debug(
                "Found cached Neovim binary for commit %s at %s",
                short_hash,
                nvim_executable,
            )
            return nvim_executable

        # Perform fresh build
        install_cache_dir = self.build_cache / short_hash
        nvim_executable = install_cache_dir / "bin" / "nvim"

        # Create temporary directory for this build (preserved for debugging)
        with tempfile.TemporaryDirectory(
            prefix=f"nvim-manager-{short_hash}-",
            delete=False,
        ) as temp_dir_str:
            temp_dir = Path(temp_dir_str)
            logger.debug(
                "Using temporary directory: %s (preserved for debugging)",
                temp_dir,
            )

            # Setup build directories
            source_dir = temp_dir / "source"
            build_dir = temp_dir / "build"

            # Clone and build
            self._shallow_clone_commit(commit_hash, source_dir)
            self._compile_and_install(source_dir, build_dir, install_cache_dir)

        # Verify executable was created
        if not nvim_executable.exists():
            msg = f"Build completed but executable not found at {nvim_executable}"
            logger.error(msg)
            raise NeovimCompileError(msg)

        logger.debug(
            "Successfully built Neovim for commit %s: %s",
            short_hash,
            nvim_executable,
        )

        self._prune_build_cache()

        return nvim_executable

    def _shallow_clone_commit(self, commit_hash: str, target_dir: Path) -> None:
        """Create a minimal clone for the specific commit.

        Uses shallow clone approach to minimize download:
        1. Shallow clone with depth=1 to get repo structure
        2. Fetch only the specific commit
        3. Checkout the commit

        This preserves git information for version generation while minimizing
        data transfer.

        Parameters
        ----------
        commit_hash : str
            Git commit hash to clone.
        target_dir : Path
            Directory to clone into.

        Raises
        ------
        NeovimCloneError
            If git operations fail.

        """
        short_hash = commit_hash[:8]
        logger.debug("Creating minimal clone for commit %s", short_hash)

        try:
            repo_url = f"https://github.com/{self.repository}.git"

            # Shallow clone to get repo structure
            repo = Repo.clone_from(repo_url, target_dir, depth=1)

            # Fetch the specific commit
            logger.debug("Fetching specific commit %s", short_hash)
            repo.git.fetch("--depth=1", "origin", commit_hash)

            # Checkout the commit
            logger.debug("Checking out commit %s", short_hash)
            repo.git.checkout(commit_hash)

            logger.debug("Shallow clone completed for commit %s", short_hash)

        except Exception as e:
            msg = f"Failed to clone commit {short_hash}: {e}"
            logger.exception(msg)
            raise NeovimCloneError(msg) from e

    def _compile_and_install(
        self,
        source_dir: Path,
        build_dir: Path,
        install_dir: Path,
    ) -> None:
        """Compile and install Neovim using the two-stage CMake process.

        Parameters
        ----------
        source_dir : Path
            Directory containing Neovim source code.
        build_dir : Path
            Directory for building.
        install_dir : Path
            Directory to install the built executable.

        Raises
        ------
        NeovimCompileError
            If any build step fails.

        """
        logger.debug("Starting compilation and install process")
        logger.debug("Source directory: %s", source_dir)
        logger.debug("Build directory: %s", build_dir)
        logger.debug("Install directory: %s", install_dir)

        build_dir.mkdir(parents=True, exist_ok=True)
        deps_build_dir = build_dir / "deps"

        # Build command sequence for Neovim two-stage process
        commands = [
            # Stage 1: Build dependencies
            [
                "cmake",
                "-S",
                str(source_dir / "cmake.deps"),
                "-B",
                str(deps_build_dir),
                "-G",
                "Ninja",
                "-D",
                "CMAKE_BUILD_TYPE=Release",
            ],
            ["cmake", "--build", str(deps_build_dir)],
            # Stage 2: Build Neovim
            [
                "cmake",
                "-S",
                str(source_dir),
                "-B",
                str(build_dir),
                "-G",
                "Ninja",
                "-D",
                "CMAKE_BUILD_TYPE=Release",
                f"-DDEPS_PREFIX={deps_build_dir}/usr",
                f"-DCMAKE_INSTALL_PREFIX={install_dir}",
            ],
            ["cmake", "--build", str(build_dir)],
            # Install Neovim
            ["cmake", "--install", str(build_dir)],
        ]

        logger.debug("Executing %d build commands", len(commands))
        logger.debug("Build logs will be preserved in %s for debugging", build_dir)

        # Execute commands with logging
        stdout_log = build_dir / "build_stdout.log"
        stderr_log = build_dir / "build_stderr.log"

        with stdout_log.open("w") as stdout_f, stderr_log.open("w") as stderr_f:
            for i, command in enumerate(commands, 1):
                logger.debug(
                    "Executing command %d/%d: %s",
                    i,
                    len(commands),
                    " ".join(command),
                )
                if not self._execute_build_command(
                    command,
                    source_dir,
                    stdout_f,
                    stderr_f,
                ):
                    msg = f"Build failed at step {i}/{len(commands)}; logs preserved in {build_dir}"
                    logger.error(msg)
                    raise NeovimCompileError(msg)

    def _execute_build_command(
        self,
        args: list[str],
        cwd: Path,
        stdout_file: TextIO,
        stderr_file: TextIO,
    ) -> bool:
        """Execute build command and preserve output for debugging.

        Parameters
        ----------
        args : list[str]
            Command and arguments to execute.
        cwd : Path
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

        # Log command execution to both files
        header = f"\n=== Executing: {command_str} ===\n"
        stdout_file.write(header)
        stderr_file.write(header)
        stdout_file.flush()
        stderr_file.flush()

        try:
            result = run_command(args, cwd=cwd, capture_output=True)
        except subprocess.CalledProcessError as e:
            # Preserve error details for debugging
            error_msg = f"Command failed with exit code {e.returncode}: {e}\n"
            stderr_file.write(error_msg)

            # Extract output from CalledProcessError
            if e.stdout:
                stdout_file.write(e.stdout)
            if e.stderr:
                stderr_file.write(e.stderr)

            stdout_file.flush()
            stderr_file.flush()
            return False
        else:
            # Write successful output
            if result.stdout:
                stdout_file.write(result.stdout)
                stdout_file.write("\n")
            if result.stderr:
                stderr_file.write(result.stderr)
                stderr_file.write("\n")
            stdout_file.flush()
            stderr_file.flush()
            return True

    def _prune_build_cache(self) -> None:
        """Remove old cached builds to stay within size limit.

        Removes the oldest builds when the total count exceeds the configured
        limit. Uses filesystem modification time to determine age.

        """
        if not self.build_cache.exists():
            return

        cached_builds = [
            d
            for d in self.build_cache.iterdir()
            if d.is_dir() and (d / "bin" / "nvim").exists()
        ]

        if len(cached_builds) <= self.build_cache_size_limit:
            return

        builds_to_remove = len(cached_builds) - self.build_cache_size_limit
        logger.debug(
            "Cache size limit exceeded (%d > %d), removing %d oldest builds",
            len(cached_builds),
            self.build_cache_size_limit,
            builds_to_remove,
        )

        cached_builds.sort(key=lambda d: d.stat().st_mtime)
        for build_dir in cached_builds[:builds_to_remove]:
            logger.debug("Removing cached build: %s", build_dir.name)
            shutil.rmtree(build_dir)

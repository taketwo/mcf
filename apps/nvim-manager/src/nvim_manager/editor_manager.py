"""Editor management operations for Neovim installations."""

import json
import re
import shutil
import tempfile
from datetime import datetime, UTC
from pathlib import Path
from typing import Any

import requests

from .config import EditorConfig
from .github import get_commit_info
from .lock_repository import LockRepository
from .logging import get_logger
from .neovim_builder import NeovimBuilder
from .utils import run_command

logger = get_logger(__name__)


class EditorManager:
    """Manages Neovim editor installation and versioning.

    Handles high-level editor operations including building from source, version
    detection, lock file management, and installation workflows. Uses NeovimBuilder
    for actual compilation and LockRepository for lock file persistence.

    Attributes
    ----------
    config : EditorConfig
        Configuration for install paths and build cache.
    lock_repo : LockRepository
        Repository for storing lock files.
    builder : NeovimBuilder
        Builder for compiling Neovim from source.

    """

    def __init__(self, config: EditorConfig, lock_repo: LockRepository) -> None:
        """Initialize EditorManager.

        Parameters
        ----------
        config : EditorConfig
            Configuration for install paths and build cache.
        lock_repo : LockRepository
            Repository for storing and retrieving lock files.

        """
        self.config = config
        self.lock_repo = lock_repo
        self.builder = NeovimBuilder(config.build_cache, config.repository)

    def update(self) -> str:
        """Update to latest Neovim version.

        Gets the latest commit from GitHub, builds it, and installs to the configured
        location.

        Returns
        -------
        str
            Commit hash that was built and installed.

        Raises
        ------
        requests.RequestException
            If GitHub API request fails.
        RuntimeError
            If build or installation fails.

        """
        logger.info("Updating editor to latest version")

        latest_commit_info = get_commit_info(self.config.repository)

        executable = self.builder.build_commit(latest_commit_info.hash)

        self._install_executable(executable)
        logger.info(
            "Successfully updated editor to commit: %s",
            latest_commit_info.hash,
        )

        return latest_commit_info.hash

    def restore(self) -> str:
        """Restore to version specified in lock file.

        Reads the lock file from repository, builds the specified commit,
        and installs to the configured location.

        Returns
        -------
        str
            Commit hash that was restored.

        Raises
        ------
        FileNotFoundError
            If lock file doesn't exist in repository.
        json.JSONDecodeError
            If lock file has invalid JSON format.
        RuntimeError
            If build or installation fails.

        """
        logger.info("Restoring from lock file: %s", self.config.lock_file)
        # Read lock file from repository
        lock_content = self.lock_repo.read_file(self.config.lock_file)
        lock_data = json.loads(lock_content)
        commit_hash = str(lock_data["hash"])
        logger.info("Restoring to commit: %s", commit_hash)

        # Build the specified commit
        executable = self.builder.build_commit(commit_hash)

        # Install from cache to final location
        self._install_executable(executable)
        logger.info("Successfully restored to commit: %s", commit_hash)

        return commit_hash

    def commit(self) -> None:
        """Save current installed version to lock file.

        Detects the currently installed Neovim version, generates
        a lock file with commit hash and UTC timestamp, and saves
        it to the repository.

        Raises
        ------
        RuntimeError
            If no Neovim installation found or version detection fails.

        """
        logger.info("Committing currently installed Neovim version to lock file")
        revision = self._parse_current_revision()
        if revision is None:
            msg = "No Neovim installation found"
            logger.error(msg)
            raise RuntimeError(msg)

        commit_info = get_commit_info(self.config.repository, revision)
        lock_data = {
            "hash": commit_info.hash,
            "date": commit_info.date.astimezone(UTC).strftime("%Y-%m-%d %H:%M:%S"),
        }

        lock_content = json.dumps(lock_data, indent=2)
        self.lock_repo.write_file(self.config.lock_file, lock_content)
        logger.info("Successfully committed revision to lock file: %s", revision)

    def status(self) -> dict[str, Any]:
        """Get current status vs lock file.

        Compares currently installed version with lock file version
        and provides status information.

        Returns
        -------
        dict[str, Any]
            Status information including current and lock versions,
            dates in local timezone for display.

        """
        logger.debug("Getting editor status")
        # Get current version
        current_revision_short = self._parse_current_revision()
        logger.debug("Current revision (short): %s", current_revision_short)

        # Expand short revision to full commit info if available
        current_revision = None
        current_date = None
        if current_revision_short:
            try:
                commit_info = get_commit_info(
                    self.config.repository,
                    current_revision_short,
                )
                current_revision = commit_info.hash
                # Store UTC date string for consistent formatting later
                current_date = commit_info.date
                logger.debug(
                    "Expanded current revision to full hash: %s",
                    current_revision,
                )
            except (requests.RequestException, KeyError) as e:
                logger.debug("Failed to expand current revision via GitHub API: %s", e)
                # Fall back to short revision for display
                current_revision = current_revision_short

        # Get lock file version
        try:
            lock_content = self.lock_repo.read_file(self.config.lock_file)
            lock_data = json.loads(lock_content)
            lock_revision = lock_data["hash"]

            # Store UTC date string - will format consistently in return
            lock_date = lock_data["date"]
            logger.debug("Lock revision: %s, date: %s", lock_revision, lock_date)
        except (FileNotFoundError, json.JSONDecodeError, KeyError) as e:
            logger.debug("No valid lock file found: %s", e)
            lock_revision = None
            lock_date = None

        # Format dates consistently for display (both to local timezone)
        formatted_current_date = None
        formatted_lock_date = None

        if current_date:
            if isinstance(current_date, datetime):
                # Already a datetime object from GitHub API
                local_date = current_date.astimezone()
                formatted_current_date = local_date.strftime("%Y-%m-%d %H:%M:%S %Z")
            else:
                # String format from fallback or legacy data
                utc_date = datetime.strptime(current_date, "%Y-%m-%d %H:%M:%S").replace(
                    tzinfo=UTC,
                )
                local_date = utc_date.astimezone()
                formatted_current_date = local_date.strftime("%Y-%m-%d %H:%M:%S %Z")

        if lock_date:
            if isinstance(lock_date, datetime):
                # Datetime object from GitHub API
                local_date = lock_date.astimezone()
                formatted_lock_date = local_date.strftime("%Y-%m-%d %H:%M:%S %Z")
            else:
                # String format from lock file
                utc_date = datetime.strptime(lock_date, "%Y-%m-%d %H:%M:%S").replace(
                    tzinfo=UTC,
                )
                local_date = utc_date.astimezone()
                formatted_lock_date = local_date.strftime("%Y-%m-%d %H:%M:%S %Z")

        return {
            "current_revision": current_revision,
            "current_date": formatted_current_date,
            "lock_revision": lock_revision,
            "lock_date": formatted_lock_date,
            "in_sync": current_revision == lock_revision if lock_revision else False,
        }

    def _get_version_string(self) -> str | None:
        """Get version string from nvim --version command.

        Returns
        -------
        str | None
            Raw output from nvim --version, or None if command fails.

        """
        logger.debug("Getting version string from: %s", self.config.install_path)
        try:
            result = run_command(
                [str(self.config.install_path), "--version"],
                capture_output=True,
            )
        except (FileNotFoundError, OSError) as e:
            logger.debug("Failed to get version: %s", e)
            return None
        else:
            return result.stdout

    def _parse_current_revision(self) -> str | None:
        """Parse revision from nvim --version output.

        Extracts commit hash from Neovim version string, matching the logic from legacy
        bash script.

        Returns
        -------
        str | None
            Commit hash or None if not found/installed.

        """
        if version_output := self._get_version_string():
            if revision := parse_neovim_version_string(version_output):
                logger.debug("Parsed revision: %s", revision)
                return revision
            logger.warning("Failed to parse revision from version output")
        return None

    def _install_executable(self, cached_executable: Path) -> None:
        """Copy executable from cache to final install location.

        Parameters
        ----------
        cached_executable : Path
            Path to built executable in cache.

        Raises
        ------
        RuntimeError
            If installation fails.

        """
        logger.debug(
            "Installing executable: %s -> %s",
            cached_executable,
            self.config.install_path,
        )
        try:
            # Ensure install directory exists
            self.config.install_path.parent.mkdir(parents=True, exist_ok=True)

            # Use atomic replacement to avoid "Text file busy" error
            # Copy to temporary file first, then move into place
            with tempfile.NamedTemporaryFile(
                dir=self.config.install_path.parent,
                delete=False,
                prefix=f".{self.config.install_path.name}.",
                suffix=".tmp",
            ) as tmp_file:
                tmp_path = Path(tmp_file.name)

            # Copy to temporary location
            shutil.copy2(cached_executable, tmp_path)
            tmp_path.chmod(0o755)

            # Atomic move - this works even if target is running
            tmp_path.replace(self.config.install_path)
            logger.debug("Executable installed successfully")
        except (OSError, shutil.Error) as e:
            msg = f"Failed to install executable: {e}"
            logger.exception("Failed to install executable")
            raise RuntimeError(msg) from e


def parse_neovim_version_string(version_output: str) -> str | None:
    """Parse commit hash from Neovim version output.

    Parameters
    ----------
    version_output : str
        Raw output from nvim --version command.

    Returns
    -------
    str | None
        Commit hash or None if not found.

    """
    version_line = version_output.split("\n")[0]
    version_string = version_line.split(" ")[1] if " " in version_line else ""

    # Try to extract revision with +g prefix (nightly builds)
    revision_match = re.search(r"\+g([0-9a-f]+)", version_string)
    if revision_match:
        return revision_match.group(1)

    # Try to extract revision with -dev- prefix (local builds)
    revision_match = re.search(r"-dev-([0-9a-f]+)", version_string)
    if revision_match:
        return revision_match.group(1)

    return None

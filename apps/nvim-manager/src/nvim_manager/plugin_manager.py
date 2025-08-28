"""Plugin management operations for Neovim plugins."""

import json
from typing import Any

from .config import PluginsConfig
from .lock_repository import LockRepository
from .logging import get_logger
from .utils import LockComparison, compare_lock_data, run_command

logger = get_logger(__name__)


class PluginManager:
    """Manages Neovim plugin installation and versioning.

    Handles plugin operations including interactive updates, restoring from lock files,
    committing current state to lock files, and status reporting. Works with Lazy.nvim
    plugin manager and lazy-lock.json format.

    Attributes
    ----------
    config : PluginsConfig
        Configuration for plugin management including retry count and local lock file path.
    lock_repo : LockRepository
        Repository for storing and retrieving lock files.

    """

    def __init__(self, config: PluginsConfig, lock_repo: LockRepository) -> None:
        """Initialize PluginManager.

        Parameters
        ----------
        config : PluginsConfig
            Configuration for plugin management.
        lock_repo : LockRepository
            Repository for storing and retrieving lock files.

        """
        self.config = config
        self.lock_repo = lock_repo

    @property
    def lock_file_name(self) -> str:
        """Get the lock file name from the local path."""
        return self.config.local_lock_path.name

    def update(self) -> None:
        """Update plugins interactively.

        Launches Neovim with Lazy plugin manager interface for interactive plugin
        updates.

        Raises
        ------
        RuntimeError
            If Neovim launch fails.

        """
        logger.info("Launching Neovim for interactive plugin updates")
        try:
            run_command(["nvim", "-c", "autocmd User VeryLazy Lazy check"])
            logger.info("Plugin update completed")
        except Exception as e:
            msg = f"Failed to launch Neovim for plugin updates: {e}"
            logger.exception("Plugin update failed")
            raise RuntimeError(msg) from e

    def restore(self) -> None:
        """Restore plugins to versions specified in lock file.

        Copies lock file from repository to local location and runs Lazy restore
        command. Includes retry logic for handling plugin restoration failures.

        Raises
        ------
        FileNotFoundError
            If lock file doesn't exist in repository.
        RuntimeError
            If restoration fails after all retry attempts.

        """
        logger.info("Restoring plugins from lock file")

        # Copy lock file from remote to local
        self.lock_repo.get_file(
            self.lock_file_name,
            self.config.local_lock_path,
        )

        # Parse expected lock data such that we can validate restoration
        expected = self._parse_lock_data(
            self.config.local_lock_path.read_text(),
        )

        # Retry Lazy plugin restoration
        differences = []
        for attempt in range(1, self.config.restore_retry_count + 1):
            logger.debug(
                "Plugin restore attempt %d/%d",
                attempt,
                self.config.restore_retry_count,
            )

            try:
                # Run headless restore commands
                run_command(
                    [
                        "nvim",
                        "--headless",
                        "+Lazy! restore",
                        "+TSUpdateSync",
                        "+qa",
                    ],
                    capture_output=True,
                )

                current = self._parse_lock_data(self.config.local_lock_path.read_text())
                if not (differences := compare_lock_data(current, expected)):
                    logger.info("Plugin restoration succeeded on attempt %d", attempt)
                    break

                if attempt != self.config.restore_retry_count:
                    logger.warning("Lock file changed during restore, retrying")

            except Exception as e:
                if attempt < self.config.restore_retry_count:
                    logger.warning("Plugin restore attempt %d failed: %s", attempt, e)
                else:
                    msg = f"Plugin restoration failed after {self.config.restore_retry_count} attempts: {e}"
                    logger.exception("Plugin restoration failed")
                    raise RuntimeError(msg) from e
        else:
            logger.error(
                "Plugin restoration failed after %d attempts",
                self.config.restore_retry_count,
            )
            for diff in differences:
                if diff.status == LockComparison.Status.DIFFERENT_VALUES:
                    logger.error(
                        "Different versions: %s (expected: %s, current: %s)",
                        diff.name,
                        diff.lock_value,
                        diff.current_value,
                    )
                elif diff.status == LockComparison.Status.MISSING_CURRENTLY:
                    logger.error("Missing plugin: %s", diff.name)
                elif diff.status == LockComparison.Status.MISSING_IN_LOCK:
                    logger.error("Unexpected plugin: %s", diff.name)

            raise RuntimeError(
                f"Plugin restoration failed after {self.config.restore_retry_count} attempts",
            )

    def commit(self) -> None:
        """Save current plugin state to lock file.

        Copies the current local lazy-lock.json to the lock repository.

        Raises
        ------
        FileNotFoundError
            If local lock file doesn't exist.
        RuntimeError
            If commit operation fails.

        """
        logger.info("Committing current plugin state to lock file")

        if not self.config.local_lock_path.exists():
            msg = f"Local lock file not found: {self.config.local_lock_path}"
            logger.error(msg)
            raise FileNotFoundError(msg)

        try:
            self.lock_repo.put_file(self.config.local_lock_path, self.lock_file_name)
            logger.info("Successfully committed plugin state to lock file")
        except Exception as e:
            msg = f"Failed to commit plugin state: {e}"
            logger.exception("Plugin commit failed")
            raise RuntimeError(msg) from e

    def status(self) -> dict[str, Any]:
        """Get current plugin status vs lock file.

        Compares the local lazy-lock.json with the version in the lock repository
        and provides detailed information about plugin differences.

        Returns
        -------
        dict[str, Any]
            Status information including sync status and detailed plugin differences.

        """
        logger.debug("Getting plugin status")

        try:
            # Parse and compare lock files
            current = self._parse_lock_data(self.config.local_lock_path.read_text())
            lock = self._parse_lock_data(self.lock_repo.read_file(self.lock_file_name))
            differences = compare_lock_data(current, lock)
            in_sync = len(differences) == 0

            return {
                "in_sync": in_sync,
                "differences": differences,
                "total_current": len(current),
                "total_lock": len(lock),
            }

        except FileNotFoundError as e:
            if "local" in str(e).lower() or str(self.config.local_lock_path) in str(e):
                return {
                    "in_sync": False,
                    "error": "Local lock file not found",
                    "differences": [],
                    "total_current": 0,
                    "total_lock": 0,
                }
            return {
                "in_sync": False,
                "error": "Remote lock file not found",
                "differences": [],
                "total_current": 0,
                "total_lock": 0,
            }
        except json.JSONDecodeError as e:
            return {
                "in_sync": False,
                "error": f"Invalid JSON in lock file: {e}",
                "differences": [],
                "total_current": 0,
                "total_lock": 0,
            }
        except Exception as e:
            logger.exception("Failed to get plugin status")
            return {
                "in_sync": False,
                "error": f"Error getting status: {e}",
                "differences": [],
                "total_current": 0,
                "total_lock": 0,
            }

    def _parse_lock_data(self, content: str) -> dict[str, str]:
        """Parse and normalize lock file content to {name: commit} format.

        Parameters
        ----------
        content : str
            Raw lock file content as JSON string.

        Returns
        -------
        dict[str, str]
            Normalized data in {name: commit} format.

        Raises
        ------
        json.JSONDecodeError
            If content is not valid JSON.

        """
        data = json.loads(content)
        return {name: info.get("commit", "unknown") for name, info in data.items()}

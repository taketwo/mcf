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
        expected_content = self.config.local_lock_path.read_text()
        expected_data = json.loads(expected_content)
        expected_normalized = {
            name: info.get("commit", "unknown") for name, info in expected_data.items()
        }

        # Retry Lazy plugin restoration
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

                # Check if restoration succeeded by comparing against expected data
                if self._compare_with_expected(expected_normalized):
                    logger.info("Plugin restoration succeeded on attempt %d", attempt)
                    break

                if attempt < self.config.restore_retry_count:
                    logger.warning("Lock file changed during restore, retrying")

            except Exception as e:
                if attempt < self.config.restore_retry_count:
                    logger.warning("Plugin restore attempt %d failed: %s", attempt, e)
                else:
                    msg = f"Plugin restoration failed after {self.config.restore_retry_count} attempts: {e}"
                    logger.exception("Plugin restoration failed")
                    raise RuntimeError(msg) from e
        else:
            # If we get here, all attempts failed
            logger.error(
                "Plugin restoration failed after %d attempts",
                self.config.restore_retry_count,
            )
            msg = "Plugin restoration failed after all retry attempts"
            raise RuntimeError(msg)

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
            # Read both files
            current_content = self.config.local_lock_path.read_text()
            lock_content = self.lock_repo.read_file(self.lock_file_name)

            # Parse JSON
            current_data = json.loads(current_content)
            lock_data = json.loads(lock_content)

            # Compare and find differences
            differences = self._find_plugin_differences(current_data, lock_data)
            in_sync = len(differences) == 0

            return {
                "in_sync": in_sync,
                "differences": differences,
                "total_current": len(current_data),
                "total_lock": len(lock_data),
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

    def _compare_with_expected(self, expected_normalized: dict[str, str]) -> bool:
        """Check if current file matches expected lock data.

        Parameters
        ----------
        expected_normalized : dict[str, str]
            Expected lock data in normalized {name: commit} format.

        Returns
        -------
        bool
            True if current file matches expected data, False otherwise.

        """
        try:
            if not self.config.local_lock_path.exists():
                return False

            current_content = self.config.local_lock_path.read_text()
            current_data = json.loads(current_content)
            current_normalized = {
                name: info.get("commit", "unknown")
                for name, info in current_data.items()
            }

            # Use existing comparison utility - empty differences means match
            differences = compare_lock_data(current_normalized, expected_normalized)
            return len(differences) == 0

        except (FileNotFoundError, json.JSONDecodeError, Exception) as e:
            logger.debug("File comparison with expected data failed: %s", e)
            return False

    def _find_plugin_differences(
        self,
        current_data: dict[str, Any],
        lock_data: dict[str, Any],
    ) -> list[LockComparison]:
        """Find differences between current and lock plugin data.

        Parameters
        ----------
        current_data : dict[str, Any]
            Current lazy-lock.json data.
        lock_data : dict[str, Any]
            Lock file lazy-lock.json data.

        Returns
        -------
        list[LockComparison]
            List of plugin differences with type-safe comparison objects.

        """
        # Normalize plugin data from {name: {commit: "hash"}} to {name: "hash"}
        normalized_current = {
            name: info.get("commit", "unknown") for name, info in current_data.items()
        }
        normalized_lock = {
            name: info.get("commit", "unknown") for name, info in lock_data.items()
        }

        return compare_lock_data(normalized_current, normalized_lock)

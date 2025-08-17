"""Tools management operations for Mason-installed tools."""

import json
import subprocess
from importlib import resources
from pathlib import Path
from typing import Any

from .config import ToolsConfig
from .lock_repository import LockRepository
from .logging import get_logger
from .utils import compare_lock_data, LockComparison, run_command

logger = get_logger(__name__)


class ToolsManager:
    """Manages Mason tools (LSP servers, linters, formatters).

    Handles tools operations including listing installed tools, committing current
    state to lock files, and status reporting. Works with Mason.nvim tool manager and
    mason-lock.json format.

    Attributes
    ----------
    config : ToolsConfig
        Configuration for tools management including lock file name.
    lock_repo : LockRepository
        Repository for storing and retrieving lock files.

    """

    def __init__(self, config: ToolsConfig, lock_repo: LockRepository) -> None:
        """Initialize ToolsManager.

        Parameters
        ----------
        config : ToolsConfig
            Configuration for tools management.
        lock_repo : LockRepository
            Repository for storing and retrieving lock files.

        """
        self.config = config
        self.lock_repo = lock_repo

    @property
    def lua_script_path(self) -> str:
        """Get path to bundled Mason tools Lua script.

        Returns
        -------
        str
            Absolute path to mason_tools.lua script.

        """
        return str(resources.files("nvim_manager") / "lua" / "mason_tools.lua")

    def _run_with_venv(
        self,
        command: list[str],
        cwd: Path | None = None,
        *,
        capture_output: bool = False,
    ) -> subprocess.CompletedProcess[str]:
        """Run command with virtual environment activated.

        Parameters
        ----------
        command : list[str]
            Command and arguments to execute.
        cwd : Path | None
            Working directory for command execution.
        capture_output : bool
            Whether to capture stdout/stderr for return.

        Returns
        -------
        subprocess.CompletedProcess[str]
            Result from run_command execution.

        """
        activate_script = self.config.venv_path / "bin" / "activate"
        if activate_script.exists():
            # Build command with venv activation using bash -c
            cmd_str = " ".join(command)
            venv_command = [
                "bash",
                "-c",
                f"source {activate_script} && {cmd_str}",
            ]
            return run_command(venv_command, cwd=cwd, capture_output=capture_output)
        # Fall back to running without venv
        logger.warning(
            "Virtual environment not found at %s, running without venv",
            self.config.venv_path,
        )
        return run_command(command, cwd=cwd, capture_output=capture_output)

    def update(self) -> None:
        """Update Mason tools interactively.

        Launches Neovim with Mason interface for interactive tool management and
        updates. Runs in the active Python environment to ensure Mason can access
        the correct pip installation.

        Raises
        ------
        RuntimeError
            If Neovim launch fails.

        """
        logger.info("Launching Neovim for interactive Mason tool updates")
        try:
            self._run_with_venv(["nvim", "-c", "Mason"])
            logger.info("Mason tool update completed")
        except Exception as e:
            msg = f"Failed to launch Neovim for Mason tool updates: {e}"
            logger.exception("Mason tool update failed")
            raise RuntimeError(msg) from e

    def restore(self) -> None:
        """Restore tools to versions specified in lock file.

        Compares currently installed tools with lock file versions and:
        - Installs tools with different versions or missing tools at lock versions
        - Uninstalls tools that are not present in lock file
        Uses Mason commands with virtualenv activation for reliable operations.

        Raises
        ------
        FileNotFoundError
            If lock file doesn't exist in repository.
        RuntimeError
            If restoration operations fail.

        """
        logger.debug("Restoring Mason tools from lock file: %s", self.config.lock_file)

        try:
            # Update Mason registry first to ensure latest package information
            logger.debug("Updating Mason registry before restoration")
            self._run_with_venv(
                ["nvim", "--headless", "+MasonUpdate", "+qa"],
                capture_output=True,
            )

            # Get current and lock data
            current_tools = self._get_installed_tools()
            lock_content = self.lock_repo.read_file(self.config.lock_file)
            lock_tools = json.loads(lock_content)

            # Find tools to install/reinstall and uninstall
            to_install = {}
            to_uninstall = []

            # Check tools in lock file
            for tool_name, lock_version in lock_tools.items():
                current_version = current_tools.get(tool_name)
                if current_version != lock_version:
                    to_install[tool_name] = lock_version
                    logger.debug(
                        "Tool %s needs install/update: current=%s, lock=%s",
                        tool_name,
                        current_version or "not installed",
                        lock_version,
                    )

            # Check tools not in lock file (to uninstall)
            for tool_name in current_tools:
                if tool_name not in lock_tools:
                    to_uninstall.append(tool_name)
                    logger.debug(
                        "Tool %s needs uninstall (not in lock file)",
                        tool_name,
                    )

            # Perform operations
            if to_install:
                logger.debug("Installing/updating %d tools", len(to_install))
                self._install_tools(to_install)

            if to_uninstall:
                logger.debug("Uninstalling %d tools", len(to_uninstall))
                self._uninstall_tools(to_uninstall)

            if not to_install and not to_uninstall:
                logger.debug("All tools are already in sync with lock file")

            logger.debug("Tool restoration completed successfully")

        except FileNotFoundError:
            msg = f"Lock file not found: {self.config.lock_file}"
            logger.exception(msg)
            raise
        except json.JSONDecodeError as e:
            msg = f"Invalid JSON in lock file {self.config.lock_file}: {e}"
            logger.exception(msg)
            raise RuntimeError(msg) from e
        except Exception as e:
            msg = f"Tool restoration failed: {e}"
            logger.debug("Tool restoration error details: %s", e)
            raise RuntimeError(msg) from e

    def commit(self) -> None:
        """Save currently installed tools to lock file.

        Gets the list of currently installed Mason tools and their versions, generates
        a lock file with tool name and version pairs, and saves it to the repository.

        Raises
        ------
        RuntimeError
            If Neovim execution fails or no tools are found.

        """
        logger.info("Committing currently installed tools to lock file")

        # Get list of installed tools from Mason
        installed_tools = self._get_installed_tools()

        if not installed_tools:
            logger.warning("No Mason tools found")
            installed_tools = {}

        # Create lock file content
        lock_content = json.dumps(installed_tools, indent=2, sort_keys=True)

        # Save to repository
        self.lock_repo.write_file(self.config.lock_file, lock_content)
        logger.info(
            "Successfully committed %d tools to lock file: %s",
            len(installed_tools),
            self.config.lock_file,
        )

    def _get_installed_tools(self) -> dict[str, str]:
        """Get list of installed Mason tools and their versions.

        Calls Neovim headless to execute MasonListInstalled command and parses
        the output to extract tool names and versions.

        Returns
        -------
        dict[str, str]
            Dictionary mapping tool names to their versions.

        Raises
        ------
        RuntimeError
            If Neovim execution fails or output cannot be parsed.

        """
        logger.debug("Getting installed Mason tools via headless Neovim")

        try:
            result = run_command(
                [
                    "nvim",
                    "--headless",
                    "-S",
                    self.lua_script_path,
                    "-c",
                    "MasonListInstalled",
                    "-c",
                    "qall!",
                ],
                capture_output=True,
            )
        except Exception as e:
            msg = f"Failed to execute Neovim headless command: {e}"
            logger.exception(msg)
            raise RuntimeError(msg) from e

        # Parse output - split on space: left=name, right=version
        # Mason output goes to stderr in headless mode
        output = (
            result.stderr.strip() if result.stderr.strip() else result.stdout.strip()
        )
        tools = {}
        for raw_line in output.split("\n"):
            line = raw_line.strip()
            if not line:
                continue

            parts = line.split(" ", 1)
            if len(parts) >= 2:  # noqa: PLR2004
                name, version = parts[0], parts[1]
                tools[name] = version
            else:
                # Handle single-part lines (tools without version)
                name = line
                version = "unknown"
                tools[name] = version

        logger.debug("Found %d installed Mason tools", len(tools))
        return tools

    def status(self) -> dict[str, Any]:
        """Get current tool status vs lock file.

        Compares the currently installed Mason tools with the version in the lock
        repository and provides detailed information about tool differences.

        Returns
        -------
        dict[str, Any]
            Status information including sync status and detailed tool differences.

        """
        logger.debug("Getting tools status")

        try:
            # Get currently installed tools
            current_data = self._get_installed_tools()

            # Read lock file
            lock_content = self.lock_repo.read_file(self.config.lock_file)
            lock_data = json.loads(lock_content)

            # Compare and find differences
            differences = self._find_tool_differences(current_data, lock_data)
            in_sync = len(differences) == 0

            return {
                "in_sync": in_sync,
                "differences": differences,
                "total_current": len(current_data),
                "total_lock": len(lock_data),
            }

        except FileNotFoundError:
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
            logger.exception("Error getting tools status")
            return {
                "in_sync": False,
                "error": f"Error getting status: {e}",
                "differences": [],
                "total_current": 0,
                "total_lock": 0,
            }

    def _find_tool_differences(
        self,
        current_data: dict[str, str],
        lock_data: dict[str, str],
    ) -> list[LockComparison]:
        """Find differences between current and lock tool data.

        Parameters
        ----------
        current_data : dict[str, str]
            Currently installed tools mapping tool names to versions.
        lock_data : dict[str, str]
            Lock file mason-lock.json data mapping tool names to versions.

        Returns
        -------
        list[LockComparison]
            List of tool differences with type-safe comparison objects.

        """
        # Tools are already in normalized format {name: version}
        return compare_lock_data(current_data, lock_data)

    def _extract_error_details(self, output: str) -> str:
        """Extract error details from Neovim/Lua script output.

        Parameters
        ----------
        output : str
            Raw output from Neovim command containing error information.

        Returns
        -------
        str
            Extracted error details in a user-friendly format.

        """
        if not output:
            return ""

        lines = output.split("\n")
        error_details = []

        # Look for our custom error messages from the Lua script
        for raw_line in lines:
            line = raw_line.strip()
            if line.startswith("Error details:"):
                # Found our error details section, collect following lines
                continue
            if line.startswith("  ") and ":" in line:
                # This looks like an error detail line: "  tool_name: error description"
                detail = line.strip()
                if detail:
                    error_details.append(detail)
            elif "missing cargo" in line.lower():
                error_details.append("missing cargo (Rust toolchain required)")
            elif "missing npm" in line.lower():
                error_details.append("missing npm (Node.js required)")
            elif "missing go" in line.lower():
                error_details.append("missing go (Go toolchain required)")
            elif "missing python" in line.lower() or "missing pip" in line.lower():
                error_details.append("missing python/pip")

        return "; ".join(error_details) if error_details else ""

    def _install_tools(self, tools: dict[str, str]) -> None:
        """Install tools using Mason with specified versions.

        Parameters
        ----------
        tools : dict[str, str]
            Dictionary mapping tool names to their target versions.

        Raises
        ------
        RuntimeError
            If installation commands fail.

        """
        if not tools:
            return

        logger.debug("Installing %d tools with versions", len(tools))

        # Create JSON specification for batch install
        tools_json = json.dumps(tools)

        try:
            self._run_with_venv(
                [
                    "nvim",
                    "--headless",
                    "-S",
                    self.lua_script_path,
                    "-c",
                    f"MasonBatchInstall {tools_json}",
                    "-c",
                    "qall!",
                ],
                capture_output=True,
            )
            logger.debug("Successfully installed %d tools", len(tools))
        except Exception as e:
            # Extract error information from the captured output
            error_output = ""
            if hasattr(e, "stderr") and e.stderr:
                error_output = (
                    e.stderr
                    if isinstance(e.stderr, str)
                    else e.stderr.decode("utf-8", errors="ignore")
                )
            elif hasattr(e, "stdout") and e.stdout:
                error_output = (
                    e.stdout
                    if isinstance(e.stdout, str)
                    else e.stdout.decode("utf-8", errors="ignore")
                )

            # Extract specific error details from Lua script output
            extracted_errors = self._extract_error_details(error_output)

            if extracted_errors:
                msg = f"Tool installation failed: {extracted_errors}"
            else:
                msg = f"Tool installation failed: {e}"

            # Log at debug level to avoid cluttering user output
            logger.debug("Tool installation error details: %s", e)
            raise RuntimeError(msg) from e

    def _uninstall_tools(self, tools: list[str]) -> None:
        """Uninstall tools using Mason.

        Parameters
        ----------
        tools : list[str]
            List of tool names to uninstall.

        Raises
        ------
        RuntimeError
            If uninstall commands fail.

        """
        if not tools:
            return

        logger.debug("Uninstalling %d tools", len(tools))

        # Create JSON specification for batch uninstall
        tools_json = json.dumps(tools)

        try:
            self._run_with_venv(
                [
                    "nvim",
                    "--headless",
                    "-S",
                    self.lua_script_path,
                    "-c",
                    f"MasonBatchUninstall {tools_json}",
                    "-c",
                    "qall!",
                ],
                capture_output=True,
            )
            logger.debug("Successfully uninstalled %d tools", len(tools))
        except Exception as e:
            # Extract error information from the captured output
            error_output = ""
            if hasattr(e, "stderr") and e.stderr:
                error_output = (
                    e.stderr
                    if isinstance(e.stderr, str)
                    else e.stderr.decode("utf-8", errors="ignore")
                )
            elif hasattr(e, "stdout") and e.stdout:
                error_output = (
                    e.stdout
                    if isinstance(e.stdout, str)
                    else e.stdout.decode("utf-8", errors="ignore")
                )

            # Extract specific error details from Lua script output
            extracted_errors = self._extract_error_details(error_output)

            if extracted_errors:
                msg = f"Tool uninstallation failed: {extracted_errors}"
            else:
                msg = f"Tool uninstallation failed: {e}"

            # Log at debug level to avoid cluttering user output
            logger.debug("Tool uninstallation error details: %s", e)
            raise RuntimeError(msg) from e

"""Tools management operations for Mason-installed tools."""

import json
from importlib import resources

from .config import ToolsConfig
from .lock_repository import LockRepository
from .logging import get_logger
from .utils import run_command

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

"""Ruff formatting hook for Python files."""

import subprocess
from typing import ClassVar

from ccu.hooks import BaseHook
from ccu.logging import get_logger

logger = get_logger(__name__)


class FormatRuffHook(BaseHook):
    """Ruff formatting hook for Python files.

    Runs Ruff formatter to ensure consistent code formatting when Python files are
    modified.

    Tool return codes:
    - 0: Formatting completed successfully
    - Other: Unexpected error (blocking)
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = [".py"]
    SUCCESS_CODES: ClassVar[list[int]] = [0]
    BLOCKING_CODES: ClassVar[list[int]] = []  # Format only has success or error
    TOOL_NAME: ClassVar[str] = "Ruff formatting"

    def _execute(self, file_path: str) -> subprocess.CompletedProcess[str]:
        """Execute Ruff format command.

        Parameters
        ----------
        file_path : str
            The Python file path to process.

        Returns
        -------
        subprocess.CompletedProcess[str]
            The result of the command execution.

        """
        return self.run_command(
            ["uv", "run", "ruff", "format", file_path],
        )

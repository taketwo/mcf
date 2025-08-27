"""Ruff auto-fix hook for Python files."""

import subprocess
from typing import ClassVar

from ccu.hooks import BaseHook
from ccu.logging import get_logger
from ccu.project import Project

logger = get_logger(__name__)


class FixRuffHook(BaseHook):
    """Ruff auto-fix hook for Python files.

    Runs Ruff auto-fix to automatically resolve linting issues when Python files are
    modified.

    Tool return codes:
    - 0: All issues fixed successfully, no diagnostics remain
    - 1: Fixes applied but unfixable issues remain (blocking)
    - Other: Unexpected error (blocking)
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = [".py"]
    SUCCESS_CODES: ClassVar[list[int]] = [0]
    BLOCKING_CODES: ClassVar[list[int]] = [1]
    TOOL_NAME: ClassVar[str] = "Ruff auto-fix"

    def __init__(self, project: Project) -> None:
        """Initialize Ruff fix hook with project context."""
        super().__init__(project)

    def _execute(self, file_path: str) -> subprocess.CompletedProcess[str]:
        """Execute Ruff auto-fix command.

        Parameters
        ----------
        file_path : str
            The Python file path to process.

        Returns
        -------
        subprocess.CompletedProcess[str]
            The result of the command execution.

        """
        return self.run_command(["uvx", "--quiet", "ruff", "check", file_path, "--fix"])

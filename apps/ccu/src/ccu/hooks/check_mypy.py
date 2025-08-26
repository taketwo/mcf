"""MyPy type checking hook for Python files."""

import subprocess
from typing import ClassVar

from ccu.hooks import BaseHook, find_project_root
from ccu.logging import get_logger

logger = get_logger(__name__)


class CheckMypyHook(BaseHook):
    """MyPy type checking hook for Python files.

    Runs MyPy type checker to validate type annotations when Python files are modified.

    Tool return codes:
    - 0: Type checking passed, no errors found
    - 1: Type errors found (blocking)
    - Other: Unexpected error (blocking)
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = [".py"]
    SUCCESS_CODES: ClassVar[list[int]] = [0]
    BLOCKING_CODES: ClassVar[list[int]] = [1]
    TOOL_NAME: ClassVar[str] = "MyPy type checking"

    def _execute(self, file_path: str) -> subprocess.CompletedProcess[str]:  # noqa: ARG002
        """Execute MyPy type checking command.

        Parameters
        ----------
        file_path : str
            The Python file path that triggered the check.

        Returns
        -------
        subprocess.CompletedProcess[str]
            The result of the command execution.

        """
        project_root = find_project_root()
        return self.run_command(
            ["uvx", "--quiet", "mypy", project_root],
        )

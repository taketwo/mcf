"""MyPy type checking hook for Python files."""

import subprocess
from typing import ClassVar

from ccu.hooks import BaseHook
from ccu.logging import get_logger
from ccu.project import Project

logger = get_logger(__name__)

# Exit code returned by uv when it cannot spawn a command
UV_SPAWN_FAILURE_EXIT_CODE = 2


class CheckMypyHook(BaseHook):
    """MyPy type checking hook for Python files.

    Runs MyPy type checker to validate type annotations when Python files are modified.

    When processing a standalone file, the tool is run in its own Python environment
    against just that file. Conversely, when processing a file that is a part of a
    Python project, the tool is run in the project's Python environment against the
    entire project.

    Tool return codes:
    - 0: Type checking passed, no errors found
    - 1: Type errors found (blocking)
    - 2: Errors preventing type checking found (blocking)
    - Other: Unexpected error (blocking)
    """

    SUPPORTED_LANGUAGES: ClassVar[list[str]] = ["python"]
    SUCCESS_CODES: ClassVar[list[int]] = [0]
    BLOCKING_CODES: ClassVar[list[int]] = [1, 2]
    TOOL_NAME: ClassVar[str] = "MyPy type checking"

    def __init__(self, project: Project) -> None:
        """Initialize MyPy hook with project context."""
        super().__init__(project)

    def _execute(self, file_path: str) -> subprocess.CompletedProcess[str]:
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
        if self.project.is_file_in_project(file_path) and self.project.has_python:
            target = str(self.project.root)
            result = self.run_command(["uv", "run", "mypy", target])
            if (
                result.returncode == UV_SPAWN_FAILURE_EXIT_CODE
                and "Failed to spawn" in result.stderr
            ):
                error_msg = (
                    "MyPy not found in project dependencies; "
                    "consider adingd it with: uv add --dev mypy"
                )
                return subprocess.CompletedProcess(
                    args=["uv", "run", "mypy", target],
                    returncode=self.BLOCKING_CODES[0],
                    stdout="",
                    stderr=error_msg,
                )
            return result
        return self.run_command(["uvx", "--quiet", "mypy", file_path])

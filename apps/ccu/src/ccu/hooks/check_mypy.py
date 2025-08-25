"""MyPy type checking hook for Python files."""

from typing import ClassVar

from ccu.hooks import BaseHook, ExitCode, HookResult, find_project_root
from ccu.logging import get_logger

logger = get_logger(__name__)


class CheckMypyHook(BaseHook):
    """MyPy type checking hook for Python files.

    This hook runs MyPy type checking when Python files are modified.
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = [".py"]

    def _execute(self, file_path: str) -> HookResult:
        """Execute MyPy type checking for the given Python file.

        Parameters
        ----------
        file_path : str
            The Python file path to process.

        Returns
        -------
        HookResult
            The result of the MyPy execution.

        """
        # Run mypy on the project directory
        project_root = find_project_root()
        result = self.run_command(
            ["uv", "run", "mypy", project_root],
        )

        if result.returncode == 0:
            # Type checking passed
            return HookResult(
                exit_code=ExitCode.SUCCESS,
                file_path=file_path,
                tool_name="MyPy type checking",
                output=result.stdout + result.stderr,
            )
        if result.returncode == 1:
            # Type errors found - blocking for Claude
            return HookResult(
                exit_code=ExitCode.BLOCKING_ERROR,
                file_path=file_path,
                tool_name="MyPy type checking",
                output=result.stdout + result.stderr,
            )
        # Other error codes (unexpected)
        logger.debug(
            "Unexpected mypy exit code for %s: %d (stdout: %r, stderr: %r)",
            file_path,
            result.returncode,
            result.stdout,
            result.stderr,
        )
        return HookResult(
            exit_code=ExitCode.BLOCKING_ERROR,
            file_path=file_path,
            tool_name="MyPy type checking",
            output=result.stdout + result.stderr,
        )

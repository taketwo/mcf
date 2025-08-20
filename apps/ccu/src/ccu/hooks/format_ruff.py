"""Ruff formatting hook for Python files."""

from typing import ClassVar

from ccu.hooks import BaseHook, ExitCode, HookResult
from ccu.logging import get_debug_logger

logger = get_debug_logger(__name__)


class FormatRuffHook(BaseHook):
    """Ruff formatting hook for Python files.

    This hook runs Ruff formatting when Python files are modified.
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = [".py"]

    def _execute(self, file_path: str) -> HookResult:
        """Execute Ruff formatting for the given Python file.

        Parameters
        ----------
        file_path : str
            The Python file path to process.

        Returns
        -------
        HookResult
            The result of the Ruff formatting execution.

        """
        format_result = self.run_command(
            ["uv", "run", "ruff", "format", file_path],
        )

        if format_result.returncode == 0:
            return HookResult(
                exit_code=ExitCode.SUCCESS,
                file_path=file_path,
                tool_name="Ruff formatting",
                output=format_result.stdout + format_result.stderr,
            )
        # Unexpected error in ruff format
        logger.debug(
            "Unexpected ruff format exit code for %s: %d (stdout: %r, stderr: %r)",
            file_path,
            format_result.returncode,
            format_result.stdout,
            format_result.stderr,
        )
        return HookResult(
            exit_code=ExitCode.BLOCKING_ERROR,
            file_path=file_path,
            tool_name="Ruff formatting",
            output=format_result.stdout + format_result.stderr,
        )

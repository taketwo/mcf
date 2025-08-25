"""Ruff auto-fix hook for Python files."""

from typing import ClassVar

from ccu.hooks import BaseHook, ExitCode, HookResult
from ccu.logging import get_logger

logger = get_logger(__name__)


class FixRuffHook(BaseHook):
    """Ruff auto-fix hook for Python files.

    This hook runs Ruff auto-fix when Python files are modified.

    Exit codes:
    - 0: All issues fixed successfully, no diagnostics remain
    - 2: Fixes applied but unfixable issues remain (blocking, diagnostics in output)
    - 2: Unexpected error occurred (blocking, error details in output)
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = [".py"]

    def _execute(self, file_path: str) -> HookResult:
        """Execute Ruff auto-fix for the given Python file.

        Parameters
        ----------
        file_path : str
            The Python file path to process.

        Returns
        -------
        HookResult
            The result of the Ruff auto-fix execution.

        """
        fix_result = self.run_command(
            ["uv", "run", "ruff", "check", file_path, "--fix"],
        )

        if fix_result.returncode == 0:
            # All issues fixed, no diagnostics left
            return HookResult(
                exit_code=ExitCode.SUCCESS,
                file_path=file_path,
                tool_name="Ruff auto-fix",
                output="",
            )
        if fix_result.returncode == 1:
            # Fixes applied but diagnostics still remain - blocking for Claude
            return HookResult(
                exit_code=ExitCode.BLOCKING_ERROR,
                file_path=file_path,
                tool_name="Ruff auto-fix",
                output=fix_result.stdout,  # Send diagnostics to output for Claude
            )
        # Other error codes (unexpected)
        logger.debug(
            "Unexpected ruff exit code for %s: %d (stdout: %r, stderr: %r)",
            file_path,
            fix_result.returncode,
            fix_result.stdout,
            fix_result.stderr,
        )
        return HookResult(
            exit_code=ExitCode.BLOCKING_ERROR,
            file_path=file_path,
            tool_name="Ruff auto-fix",
            output=fix_result.stderr or fix_result.stdout,
        )

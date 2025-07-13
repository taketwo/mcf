"""Utility functions for subprocess execution."""

import subprocess
from pathlib import Path


def run_command(
    args: list[str],
    cwd: Path | None = None,
    *,
    capture_output: bool = False,
) -> subprocess.CompletedProcess[str]:
    """Run shell command with proper error handling.

    Parameters
    ----------
    args : list[str]
        Command and arguments to execute.
    cwd : Path | None
        Working directory for command execution.
    capture_output : bool
        Whether to capture stdout/stderr for return.

    Returns
    -------
    subprocess.CompletedProcess[str]
        Completed process with return code and output.

    Raises
    ------
    subprocess.CalledProcessError
        If command exits with non-zero return code.

    """
    return subprocess.run(
        args,
        cwd=cwd,
        capture_output=capture_output,
        text=True,
        check=True,
    )

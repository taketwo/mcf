"""Shared utilities for Claude Code hooks.

This module provides common functionality for hooks that process file changes
and run development tools like linting, type checking, and formatting.
"""

import json
import subprocess
import sys
from pathlib import Path


def parse_hook_input() -> str:
    """Parse JSON input from stdin and extract file_path.

    Returns
    -------
    str
        The file path from the tool input, or empty string if not found.

    Raises
    ------
    SystemExit
        With code 2 if JSON parsing fails.

    """
    try:
        input_data = json.load(sys.stdin)
        tool_input = input_data.get("tool_input", {})
        return str(tool_input.get("file_path", ""))
    except (json.JSONDecodeError, KeyError) as e:
        print(f"Error parsing hook input: {e}", file=sys.stderr)
        sys.exit(2)


def run_command(
    command: list[str],
    cwd: Path | None = None,
    description: str = "",
) -> subprocess.CompletedProcess[str]:
    """Run a command with proper error handling.

    Parameters
    ----------
    command : list[str]
        The command to run as a list of strings.
    cwd : Path | None, optional
        Working directory to run the command in.
    description : str, optional
        Description of the command for error messages.

    Returns
    -------
    subprocess.CompletedProcess[str]
        The completed process result.

    Raises
    ------
    SystemExit
        With code 2 if command execution fails.

    """
    try:
        if cwd is None:
            cwd = Path(__file__).parent.parent.parent  # Project root

        return subprocess.run(
            command,
            check=False,
            capture_output=True,
            text=True,
            cwd=cwd,
        )
    except (OSError, subprocess.SubprocessError) as e:
        print(f"Error running {description or ' '.join(command)}: {e}", file=sys.stderr)
        sys.exit(2)


def exit_with_message(
    *,
    success: bool,
    file_path: str,
    tool_name: str,
    stdout: str = "",
    stderr: str = "",
) -> None:
    """Exit with appropriate code and message.

    Parameters
    ----------
    success : bool
        Whether the operation was successful.
    file_path : str
        The file path that was processed.
    tool_name : str
        Name of the tool that was run.
    stdout : str, optional
        Standard output from the tool.
    stderr : str, optional
        Standard error from the tool.

    """
    if success:
        print(f"{tool_name} passed for {file_path}")
        sys.exit(0)
    else:
        print(f"{tool_name} failed for {file_path}:", file=sys.stderr)
        if stdout:
            print(stdout, file=sys.stderr)
        if stderr:
            print(stderr, file=sys.stderr)
        sys.exit(2)

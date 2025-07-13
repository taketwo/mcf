#!/usr/bin/env python

"""MyPy type checking hook for Claude Code.

This hook runs mypy type checking when Python files are modified. If type errors are
found, it exits with code 2 to provide blocking feedback to Claude.
"""

import sys

from hook_utils import exit_with_message, parse_hook_input, run_command


def main() -> None:
    """Process the tool input and runs mypy if needed."""
    # Parse input and get file path
    file_path = parse_hook_input()

    # Only run mypy for Python files
    if not file_path.endswith(".py"):
        sys.exit(0)

    # Run mypy on the src directory
    result = run_command(
        ["uv", "run", "mypy", "src/"],
        description="mypy",
    )

    # Exit with appropriate code and message
    success = result.returncode == 0
    exit_with_message(
        success=success,
        file_path=file_path,
        tool_name="MyPy type checking",
        stdout=result.stdout,
        stderr=result.stderr,
    )


if __name__ == "__main__":
    main()

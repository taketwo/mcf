#!/usr/bin/env python

"""Ruff linting hook for Claude Code.

This hook runs Ruff linting when Python files are modified. It first attempts
to auto-fix issues, then checks for remaining problems. If unfixable issues
remain, it exits with code 2 to provide blocking feedback to Claude.
"""

import sys

from hook_utils import exit_with_message, parse_hook_input, run_command


def main() -> None:
    """Process the tool input and runs Ruff if needed."""
    # Parse input and get file path
    file_path = parse_hook_input()

    # Only run Ruff for Python files
    if not file_path.endswith(".py"):
        sys.exit(0)

    # First, try to auto-fix issues
    run_command(
        ["uv", "run", "ruff", "check", "src/", "--fix"],
        description="ruff check --fix",
    )

    # Then check for remaining issues
    check_result = run_command(
        ["uv", "run", "ruff", "check", "src/"],
        description="ruff check",
    )

    # Exit with appropriate code and message
    success = check_result.returncode == 0
    exit_with_message(
        success=success,
        file_path=file_path,
        tool_name="Ruff linting",
        stdout=check_result.stdout,
        stderr=check_result.stderr,
    )


if __name__ == "__main__":
    main()

"""Utility functions for subprocess execution and lock data comparison."""

import subprocess
from dataclasses import dataclass
from enum import Enum
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


@dataclass(frozen=True)
class LockComparison:
    """Represents a comparison result between current and lock data.

    Attributes
    ----------
    name : str
        Name of the item being compared.
    status : Status
        Comparison status indicating the type of difference found.
    current_value : str | None
        Current value, if present.
    lock_value : str | None
        Lock file value, if present.

    """

    class Status(Enum):
        """Status values for lock data comparison results."""

        MISSING_CURRENTLY = "missing_currently"
        MISSING_IN_LOCK = "missing_in_lock"
        DIFFERENT_VALUES = "different_values"

    name: str
    status: Status
    current_value: str | None = None
    lock_value: str | None = None


def compare_lock_data(
    current_data: dict[str, str],
    lock_data: dict[str, str],
) -> list[LockComparison]:
    """Compare current and lock data to find differences.

    Parameters
    ----------
    current_data : dict[str, str]
        Currently installed items in normalized {name: value} format.
    lock_data : dict[str, str]
        Lock file items in normalized {name: value} format.

    Returns
    -------
    list[LockComparison]
        List of differences with type-safe comparison objects.

    """
    differences = []
    all_items = set(current_data.keys()) | set(lock_data.keys())

    for item in sorted(all_items):
        current_value = current_data.get(item)
        lock_value = lock_data.get(item)

        if current_value is None:
            differences.append(
                LockComparison(
                    name=item,
                    status=LockComparison.Status.MISSING_CURRENTLY,
                    lock_value=lock_value,
                ),
            )
        elif lock_value is None:
            differences.append(
                LockComparison(
                    name=item,
                    status=LockComparison.Status.MISSING_IN_LOCK,
                    current_value=current_value,
                ),
            )
        elif current_value != lock_value:
            differences.append(
                LockComparison(
                    name=item,
                    status=LockComparison.Status.DIFFERENT_VALUES,
                    current_value=current_value,
                    lock_value=lock_value,
                ),
            )

    return differences

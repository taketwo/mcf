"""Hook classes for Claude Code integration.

This module provides a base hook class and derived implementations for various
development tools like linting and type checking. The classes are designed to
be used by a superapp that handles hook execution.
"""

import importlib
import os
import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
from typing import Any, ClassVar, cast


class ExitCode(IntEnum):
    """Exit codes for hook execution."""

    SUCCESS = 0
    NON_BLOCKING_ERROR = 1
    BLOCKING_ERROR = 2


class UnknownHookError(Exception):
    """Raised when an unknown hook is requested."""

    def __init__(self, hook_name: str) -> None:
        """Initialize the exception with the unknown hook name."""
        self.hook_name = hook_name
        super().__init__(f"Unknown hook: {hook_name}")


@dataclass
class HookResult:
    """Result of a hook execution.

    Parameters
    ----------
    exit_code : int
        Exit code from the hook execution (0=success, 2=blocking error, other=non-blocking error).
    file_path : str
        The file path that was processed.
    tool_name : str
        Name of the tool that was executed.
    output : str
        Combined output from the tool execution.

    """

    exit_code: int
    file_path: str
    tool_name: str
    output: str = ""


def find_project_root() -> str:
    """Find the project root directory.

    Currently uses CLAUDE_PROJECT_DIR environment variable.

    TODO: Improve this to look for root markers such as .git, pyproject.toml,
    setup.py, etc.
    """
    return os.environ.get("CLAUDE_PROJECT_DIR", ".")


class BaseHook(ABC):
    """Abstract base class for all hooks.

    This class provides common functionality for parsing input, filtering files,
    and orchestrating hook execution. Derived classes need to define supported
    file extensions and implement the actual hook logic.
    """

    SUPPORTED_EXTENSIONS: ClassVar[list[str]] = []

    def run_command(
        self,
        command: list[str],
        cwd: Path | None = None,
    ) -> subprocess.CompletedProcess[str]:
        """Run a command with proper error handling.

        Parameters
        ----------
        command : list[str]
            The command to run as a list of strings.
        cwd : Path | None, optional
            Working directory to run the command in.

        Returns
        -------
        subprocess.CompletedProcess[str]
            The completed process result.

        Raises
        ------
        OSError
            If the command cannot be executed.
        subprocess.SubprocessError
            If there are issues with subprocess execution.

        """
        if cwd is None:
            cwd = Path(find_project_root())

        return subprocess.run(
            command,
            check=False,
            capture_output=True,
            text=True,
            cwd=cwd,
        )

    def should_execute(self, file_path: str) -> bool:
        """Check if this hook should execute for the given file.

        Parameters
        ----------
        file_path : str
            The file path to check.

        Returns
        -------
        bool
            True if the hook should execute, False otherwise.

        """
        if not file_path:
            return False

        file_extension = Path(file_path).suffix
        return file_extension in self.SUPPORTED_EXTENSIONS

    def execute(self, input_data: dict[str, Any]) -> HookResult:
        """Execute the hook with input parsing and validation.

        Parameters
        ----------
        input_data : dict[str, Any]
            Pre-parsed hook input data from Claude Code.

        Returns
        -------
        HookResult
            The result of the hook execution.

        """
        file_path = input_data["tool_input"]["file_path"]

        if not self.should_execute(file_path):
            # Return success for files we don't process
            return HookResult(
                exit_code=0,
                file_path=file_path,
                tool_name=self.__class__.__name__,
                output="",
            )

        return self._execute(file_path)

    @abstractmethod
    def _execute(self, file_path: str) -> HookResult:
        """Execute the hook logic for the given file.

        This method must be implemented by derived classes to perform the
        actual hook operation (linting, type checking, etc.).

        Parameters
        ----------
        file_path : str
            The file path to process.

        Returns
        -------
        HookResult
            The result of the hook execution.

        """


def list_available_hooks() -> list[str]:
    """List all available hook modules in the hooks directory.

    Returns
    -------
    list[str]
        Sorted list of available hook names (e.g., ['check-mypy', 'fix-ruff']).

    """
    hooks_dir = Path(__file__).parent
    available_hooks = []

    for file_path in hooks_dir.glob("*.py"):
        if file_path.name.startswith("_"):
            continue

        module_name = file_path.stem
        hook_name = module_name.replace("_", "-")

        try:
            get_hook(hook_name)  # this validates the hook exists and works
            available_hooks.append(hook_name)
        except UnknownHookError:
            continue

    return sorted(available_hooks)


def get_hook(hook_name: str) -> BaseHook:
    """Get a hook instance by name.

    Parameters
    ----------
    hook_name : str
        Name of the hook (e.g., "fix-ruff", "format-ruff", "check-mypy").

    Returns
    -------
    BaseHook
        Instance of the requested hook.

    Raises
    ------
    UnknownHookError
        If the hook name is not recognized.

    """
    try:
        module_name = hook_name.replace("-", "_")
        class_name = (
            "".join(word.capitalize() for word in hook_name.split("-")) + "Hook"
        )
        module = importlib.import_module(f".{module_name}", package=__package__)
        hook_class = getattr(module, class_name)
        return cast("BaseHook", hook_class())
    except (ImportError, AttributeError) as e:
        raise UnknownHookError(hook_name) from e

"""Hook classes for Claude Code integration.

This module provides a base hook class and derived implementations for various
development tools like linting and type checking. The classes are designed to
be used by a superapp that handles hook execution.
"""

import importlib
import subprocess
from abc import ABC, abstractmethod
from dataclasses import dataclass
from enum import IntEnum
from pathlib import Path
from typing import Any, ClassVar, cast

from ccu.languages import is_file_in_language
from ccu.logging import get_logger
from ccu.project import Project

logger = get_logger(__name__)


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


class BaseHook(ABC):
    """Abstract base class for all hooks.

    This class provides common functionality for parsing input, filtering files,
    and orchestrating hook execution. Derived classes need to define supported
    languages and implement the actual hook logic.
    """

    SUPPORTED_LANGUAGES: ClassVar[list[str]] = []
    SUCCESS_CODES: ClassVar[list[int]] = []
    BLOCKING_CODES: ClassVar[list[int]] = []
    TOOL_NAME: ClassVar[str] = ""

    def __init__(self, project: Project) -> None:
        """Initialize hook with project context.

        Parameters
        ----------
        project : Project
            The project context for this hook execution.

        """
        self.project = project

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
            cwd = self.project.root

        cmd_str = " ".join(command)
        logger.debug("Running command: %s in %s", cmd_str, cwd)

        try:
            result = subprocess.run(
                command,
                check=False,
                capture_output=True,
                text=True,
                cwd=cwd,
            )
            logger.debug(
                "Command completed: exit_code=%d, stdout_len=%d, stderr_len=%d",
                result.returncode,
                len(result.stdout),
                len(result.stderr),
            )
            if result.returncode != 0:
                logger.info(
                    "Command failed with exit code %d: %s",
                    result.returncode,
                    cmd_str,
                )
        except OSError:
            logger.exception("Failed to execute command %s", cmd_str)
            raise
        except subprocess.SubprocessError:
            logger.exception("Subprocess error for command %s", cmd_str)
            raise
        else:
            return result

    def should_execute(self, file_path: Path) -> bool:
        """Check if this hook should execute for the given file.

        Parameters
        ----------
        file_path : Path
            The file path to check.

        Returns
        -------
        bool
            True if the hook should execute, False otherwise.

        """
        if not file_path:
            logger.warning("Empty file path provided to %s", self.__class__.__name__)
            return False

        return any(
            is_file_in_language(file_path, language)
            for language in self.SUPPORTED_LANGUAGES
        )

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
        logger.info("Executing %s for file: %s", self.__class__.__name__, file_path)

        if not self.should_execute(Path(file_path)):
            return HookResult(
                exit_code=0,
                file_path=file_path,
                tool_name=self.__class__.__name__,
                output="",
            )

        try:
            result = self._execute(file_path)
            tool_name = self.TOOL_NAME or self.__class__.__name__

            if result.returncode in self.SUCCESS_CODES:
                logger.info("%s completed successfully for: %s", tool_name, file_path)
                return HookResult(
                    exit_code=ExitCode.SUCCESS,
                    file_path=file_path,
                    tool_name=tool_name,
                    output=result.stdout + result.stderr,
                )
            if result.returncode in self.BLOCKING_CODES:
                logger.info("%s found issues for: %s (blocking)", tool_name, file_path)
                return HookResult(
                    exit_code=ExitCode.BLOCKING_ERROR,
                    file_path=file_path,
                    tool_name=tool_name,
                    output=result.stdout + result.stderr,
                )
            logger.warning(
                "Unexpected %s exit code %d for %s",
                tool_name.lower(),
                result.returncode,
                file_path,
            )
            return HookResult(
                exit_code=ExitCode.BLOCKING_ERROR,
                file_path=file_path,
                tool_name=tool_name,
                output=result.stderr or result.stdout,
            )
        except Exception:
            logger.exception(
                "Unexpected error in %s._execute for %s",
                self.__class__.__name__,
                file_path,
            )
            raise

    @abstractmethod
    def _execute(self, file_path: str) -> subprocess.CompletedProcess[str]:
        """Execute the hook logic for the given file.

        This method must be implemented by derived classes to perform the
        actual hook operation (linting, type checking, etc.).

        Parameters
        ----------
        file_path : str
            The file path to process.

        Returns
        -------
        subprocess.CompletedProcess[str]
            The result of the command execution.

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
    logger.debug("Scanning hooks directory: %s", hooks_dir)

    for file_path in hooks_dir.glob("*.py"):
        if file_path.name.startswith("_"):
            logger.debug("Skipping private module: %s", file_path.name)
            continue

        module_name = file_path.stem
        hook_name = module_name.replace("_", "-")
        logger.debug("Checking hook: %s (from %s)", hook_name, file_path.name)

        try:
            # Create temporary project for validation
            temp_project = Project(Path())
            get_hook(
                hook_name,
                temp_project,
            )  # this validates the hook exists and works
            available_hooks.append(hook_name)
            logger.debug("Hook %s is available", hook_name)
        except UnknownHookError:
            logger.debug("Hook %s is not valid", hook_name)
            continue

    logger.info(
        "Found %d available hooks: %s",
        len(available_hooks),
        sorted(available_hooks),
    )
    return sorted(available_hooks)


def get_hook(hook_name: str, project: Project) -> BaseHook:
    """Get a hook instance by name.

    Parameters
    ----------
    hook_name : str
        Name of the hook (e.g., "fix-ruff", "format-ruff", "check-mypy").
    project : Project
        The project context to pass to the hook constructor.

    Returns
    -------
    BaseHook
        Instance of the requested hook.

    Raises
    ------
    UnknownHookError
        If the hook name is not recognized.

    """
    logger.debug("Loading hook: %s", hook_name)
    try:
        module_name = hook_name.replace("-", "_")
        class_name = (
            "".join(word.capitalize() for word in hook_name.split("-")) + "Hook"
        )
        logger.debug("Importing module: %s, class: %s", module_name, class_name)
        module = importlib.import_module(f".{module_name}", package=__package__)
        hook_class = getattr(module, class_name)
        hook_instance = cast("BaseHook", hook_class(project))
        logger.debug("Successfully loaded hook: %s", hook_name)
    except (ImportError, AttributeError) as e:
        logger.debug("Failed to load hook %s: %s", hook_name, e)
        raise UnknownHookError(hook_name) from e
    else:
        return hook_instance

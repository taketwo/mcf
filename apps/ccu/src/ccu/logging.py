"""Logging configuration for CCU.

This module provides centralized logging configuration, adapting the output format and
behavior based on whether the application is running in an interactive or
non-interactive context.
"""

import logging
import os
import sys
import tempfile
from pathlib import Path
from typing import Any

from rich.console import Console
from rich.logging import RichHandler

# Re-export logging levels
DEBUG = logging.DEBUG
INFO = logging.INFO
WARNING = logging.WARNING
ERROR = logging.ERROR
CRITICAL = logging.CRITICAL


class InteractiveLogHandler(RichHandler):
    """Flexible RichHandler for interactive terminal output.

    Configurable to provide either concise user-friendly output or detailed debug output
    with timestamps, levels, and module information.
    """

    def __init__(self, *, concise: bool = True) -> None:
        """Initialize the handler with formatting options.

        Parameters
        ----------
        concise : bool, optional
            Whether to use concise formatting (minimal) or detailed formatting, by
            default True

        """
        super().__init__(
            console=Console(stderr=True),
            rich_tracebacks=not concise,
            tracebacks_show_locals=not concise,
            show_time=not concise,
            show_level=not concise,
            show_path=not concise,
            markup=True,
        )
        self._concise = concise

    def format(self, record: logging.LogRecord) -> str:
        """Format the log record based on concise mode."""
        if self._concise:
            message = record.getMessage()
            if record.levelno >= logging.ERROR:
                return f"[red]{message}[/red]"
            if record.levelno >= logging.WARNING:
                return f"[yellow]{message}[/yellow]"
            return message
        return super().format(record)


class NonInteractiveLogHandler(logging.StreamHandler[Any]):
    """Simplified handler for non-interactive operation.

    Provides clean, parseable output suitable for log collection systems and file
    logging.
    """

    def __init__(self) -> None:
        """Initialize the handler with a basic formatter."""
        super().__init__(sys.stdout)
        self.setFormatter(
            logging.Formatter(
                "%(asctime)s [%(levelname)s] %(name)s: %(message)s",
                datefmt="%Y-%m-%d %H:%M:%S",
            ),
        )


class DebugFileLogHandler(logging.FileHandler):
    """Debug file log handler that writes to a project-relative or configured file."""

    def __init__(self) -> None:
        """Initialize the handler with debug log file."""
        if debug_file_env := os.environ.get("CCU_DEBUG_FILE"):
            debug_log_path = Path(debug_file_env)
        else:
            current_dir = Path.cwd()
            for parent in [current_dir, *current_dir.parents]:
                if (parent / ".git").exists() or (parent / "pyproject.toml").exists():
                    debug_log_path = parent / ".ccu-debug.log"
                    break
            else:
                debug_log_path = Path(tempfile.gettempdir()) / "ccu-debug.log"
        super().__init__(debug_log_path)
        self.setLevel(logging.DEBUG)
        self.setFormatter(
            logging.Formatter(
                "%(asctime)s [%(levelname)s] %(name)s: %(message)s",
                datefmt="%Y-%m-%d %H:%M:%S",
            ),
        )


def configure_logging(*, debug: bool = False) -> None:
    """Configure application-wide logging behavior.

    The behavior is contextual based on the execution environment:

    Interactive mode (terminal/tty):
    - debug=False: INFO+ to console with rich formatting
    - debug=True: DEBUG+ to console with rich formatting

    Non-interactive mode (ClaudeCode hook):
    - debug=False: ERROR+ to stdout/stderr only
    - debug=True: ERROR+ to stdout/stderr and DEBUG+ to debug file

    Parameters
    ----------
    debug : bool, optional
        Enable debug behavior, by default False

    """
    # Remove any existing handlers
    root_logger = logging.getLogger()
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    # Configure based on execution context
    if sys.stdout.isatty() and sys.stderr.isatty():
        console_level = DEBUG if debug else INFO
        root_logger.setLevel(console_level)
        handler = InteractiveLogHandler(concise=not debug)
        handler.setLevel(console_level)
        root_logger.addHandler(handler)
    else:
        root_logger.setLevel(DEBUG if debug else ERROR)
        error_handler = NonInteractiveLogHandler()
        error_handler.setLevel(ERROR)
        root_logger.addHandler(error_handler)
        if debug:
            debug_handler = DebugFileLogHandler()
            root_logger.addHandler(debug_handler)


def get_logger(name: str) -> logging.Logger:
    """Get a logger with the given name.

    This is a convenience wrapper around logging.getLogger that ensures consistent
    logger naming throughout the application.

    Parameters
    ----------
    name : str
        The logger name, typically __name__ from the calling module

    Returns
    -------
    logging.Logger
        A configured logger instance with appropriate namespace

    """
    if name.startswith("ccu."):
        return logging.getLogger(name)
    return logging.getLogger(f"ccu.{name}")

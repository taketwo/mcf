"""Logging configuration for nvim-manager.

This module provides centralized logging configuration, adapting the output format and
behavior based on whether the application is running in an interactive or
non-interactive context.
"""

import logging
import sys
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

    Configurable to provide either concise user-friendly output or detailed
    debug output with timestamps, levels, and module information.
    """

    def __init__(self, *, concise: bool = True) -> None:
        """Initialize the handler with formatting options.

        Parameters
        ----------
        concise : bool, optional
            Whether to use concise formatting (minimal) or detailed formatting, by default True

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


def configure_logging(level: int = logging.INFO, *, concise: bool = True) -> None:
    """Configure application-wide logging behavior.

    Parameters
    ----------
    level : int, optional
        The logging level to use, by default logging.INFO
    concise : bool, optional
        Whether to use concise formatting (minimal) or detailed formatting, by default True

    """
    # Remove any existing handlers
    root_logger = logging.getLogger()
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    # Set basic configuration
    root_logger.setLevel(level)

    # Add appropriate handler based on environment and concise mode
    if sys.stdout.isatty() and sys.stderr.isatty():
        handler = InteractiveLogHandler(concise=concise)
    else:
        handler = NonInteractiveLogHandler()

    root_logger.addHandler(handler)


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
    if name.startswith("nvim_manager."):
        return logging.getLogger(name)
    return logging.getLogger(f"nvim_manager.{name}")

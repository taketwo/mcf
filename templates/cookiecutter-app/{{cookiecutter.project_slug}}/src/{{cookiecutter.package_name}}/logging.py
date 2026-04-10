"""Logging configuration for {{ cookiecutter.project_name }}."""

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


def configure_logging(*, debug: bool = False) -> None:
    """Configure application-wide logging behavior.

    Interactive mode (terminal/tty):
    - debug=False: INFO+ to console with concise rich formatting
    - debug=True: DEBUG+ to console with detailed rich formatting

    Non-interactive mode:
    - debug=False: INFO+ to stdout with plain formatting
    - debug=True: DEBUG+ to stdout with plain formatting

    Parameters
    ----------
    debug : bool, optional
        Enable debug logging, by default False

    """
    root_logger = logging.getLogger()
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    root_logger.setLevel(DEBUG if debug else INFO)

    if sys.stdout.isatty() and sys.stderr.isatty():
        handler: logging.Handler = InteractiveLogHandler(concise=not debug)
    else:
        handler = NonInteractiveLogHandler()

    handler.setLevel(DEBUG if debug else INFO)
    root_logger.addHandler(handler)


def get_logger(name: str) -> logging.Logger:
    """Get a logger with the given name.

    Parameters
    ----------
    name : str
        The logger name, typically __name__ from the calling module

    Returns
    -------
    logging.Logger
        A configured logger instance

    """
    if name.startswith("{{ cookiecutter.package_name }}."):
        return logging.getLogger(name)
    return logging.getLogger(f"{{ cookiecutter.package_name }}.{name}")

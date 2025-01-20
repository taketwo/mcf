"""Logging configuration for BAM."""

import logging
import sys
from typing import ClassVar

from colorama import Fore, Style

from rich.console import Console
from rich.logging import RichHandler


class ColorFormatter(logging.Formatter):
    """Simple color formatter using colorama."""

    COLORS: ClassVar[dict[int, str]] = {
        logging.DEBUG: Fore.BLUE,
        logging.INFO: "",
        logging.WARNING: Fore.YELLOW,
        logging.ERROR: Fore.RED,
        logging.CRITICAL: Fore.RED + Style.BRIGHT,
    }

    def format(self, record: logging.LogRecord) -> str:
        """Format log record with color based on log level."""
        color = self.COLORS.get(record.levelno, "")
        message = super().format(record)
        return f"{color}{message}{Style.RESET_ALL}"


def configure_logging(*, debug: bool = False) -> None:
    """Configure logging based on mode.

    Parameters
    ----------
    debug : bool, optional
        Enable debug logging level and use Rich formatting.

    """
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.DEBUG if debug else logging.INFO)

    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    if debug:
        console = Console(force_terminal=True)
        rich_handler = RichHandler(
            console=console,
            show_time=True,
            show_path=True,
            enable_link_path=True,
        )
        rich_handler.setFormatter(logging.Formatter("%(message)s"))
        root_logger.addHandler(rich_handler)
    else:
        stdout_handler = logging.StreamHandler(sys.stdout)
        stdout_handler.setFormatter(ColorFormatter("%(message)s"))
        stdout_handler.addFilter(lambda record: record.levelno <= logging.INFO)
        root_logger.addHandler(stdout_handler)
        stderr_handler = logging.StreamHandler(sys.stderr)
        stderr_handler.setFormatter(ColorFormatter("%(message)s"))
        stderr_handler.setLevel(logging.WARNING)
        root_logger.addHandler(stderr_handler)


def get_logger(name: str) -> logging.Logger:
    """Get logger instance with given name."""
    return logging.getLogger(name)

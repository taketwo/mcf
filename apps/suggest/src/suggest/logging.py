"""Logging configuration for suggest.

This module provides centralized logging configuration that writes to a file
in the XDG state directory to avoid interfering with interactive UI.
"""

import logging
import os
from pathlib import Path

# Re-export logging levels
DEBUG = logging.DEBUG
INFO = logging.INFO
WARNING = logging.WARNING
ERROR = logging.ERROR
CRITICAL = logging.CRITICAL


def get_log_file_path() -> Path:
    """Get the path to the log file using XDG Base Directory specification.

    Returns
    -------
    Path
        Path to the log file, typically ~/.local/state/suggest/debug.log

    """
    xdg_state_home = os.environ.get("XDG_STATE_HOME")
    if xdg_state_home:
        state_dir = Path(xdg_state_home)
    else:
        state_dir = Path.home() / ".local" / "state"

    log_dir = state_dir / "suggest"
    log_dir.mkdir(parents=True, exist_ok=True)

    return log_dir / "debug.log"


def configure_logging() -> Path:
    """Configure application-wide logging behavior.

    Logs are written to a file in XDG_STATE_HOME to avoid interfering
    with the interactive terminal UI.

    Configuration:
    - Root logger: INFO level (for third-party libraries)
    - suggest.* loggers: DEBUG level (for application code)

    Returns
    -------
    Path
        Path to the log file where logs are being written

    """
    # Remove any existing handlers
    root_logger = logging.getLogger()
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)

    # Set root logger to INFO to reduce noise from third-party libraries
    root_logger.setLevel(logging.INFO)

    # Get log file path
    log_file = get_log_file_path()

    # Add file handler
    handler = logging.FileHandler(log_file, mode="a", encoding="utf-8")
    handler.setFormatter(
        logging.Formatter(
            "%(asctime)s [%(levelname)s] %(name)s: %(message)s",
            datefmt="%Y-%m-%d %H:%M:%S",
        ),
    )

    root_logger.addHandler(handler)

    # Set our application logger to DEBUG to capture all our logs
    suggest_logger = logging.getLogger("suggest")
    suggest_logger.setLevel(logging.DEBUG)

    return log_file


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
    if name.startswith("suggest."):
        return logging.getLogger(name)
    return logging.getLogger(f"suggest.{name}")

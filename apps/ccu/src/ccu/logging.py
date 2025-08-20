"""Logging utilities for CCU hooks."""

import logging
import tempfile
from pathlib import Path


def get_debug_logger(name: str) -> logging.Logger:
    """Get a debug logger that writes to a temp debug log file.

    Parameters
    ----------
    name : str
        Logger name (typically __name__).

    Returns
    -------
    logging.Logger
        Configured debug logger.

    """
    logger = logging.getLogger(name)

    # Only configure if not already configured
    if not logger.handlers:
        debug_log_path = Path(tempfile.gettempdir()) / "ccu-hooks-debug.log"

        # Create handler
        handler = logging.FileHandler(debug_log_path)
        handler.setLevel(logging.DEBUG)

        # Create formatter
        formatter = logging.Formatter(
            "%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        )
        handler.setFormatter(formatter)

        # Add handler to logger
        logger.addHandler(handler)
        logger.setLevel(logging.DEBUG)

    return logger

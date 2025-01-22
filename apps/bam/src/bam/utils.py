from __future__ import annotations

import subprocess

from .logging import logging

logger = logging.getLogger(__name__)


def run_command(command: str) -> tuple[str, str]:
    """Run command and return its output."""
    logger.debug("Running command: %s", command)
    process = subprocess.Popen(
        command,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        shell=True,
        text=True,
    )
    stdout, stderr = process.communicate()
    if process.returncode != 0:
        logger.error(
            "Command %s failed with return code %d",
            command,
            process.returncode,
        )
        if stdout:
            logger.error("Stdout: %s", stdout.strip())
        if stderr:
            logger.error("Stderr: %s", stderr.strip())
    return stdout, stderr

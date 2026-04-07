from __future__ import annotations

import subprocess

from .logging import logging

logger = logging.getLogger(__name__)


class CommandError(Exception):
    """External command exited with non-zero status."""

    def __init__(
        self,
        command: str,
        returncode: int,
        stdout: str,
        stderr: str,
    ) -> None:
        self.command = command
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr
        super().__init__(
            f"Command failed with exit code {returncode}: {command}",
        )


def run_command(command: str) -> tuple[str, str, int]:
    """Run command and return its output and exit code."""
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
    return stdout, stderr, process.returncode


def run_command_check(command: str) -> tuple[str, str]:
    """Run command and raise CommandError if it exits non-zero."""
    stdout, stderr, returncode = run_command(command)
    if returncode != 0:
        raise CommandError(command, returncode, stdout, stderr)
    return stdout, stderr

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

import pytest
from click.testing import CliRunner

if TYPE_CHECKING:
    from collections.abc import Generator


@pytest.fixture(autouse=True)
def restore_root_logger() -> Generator[None]:
    """Restore root logger state after each test.

    Prevents logging configuration from leaking between tests.
    """
    root_logger = logging.getLogger()
    original_level = root_logger.level
    original_handlers = root_logger.handlers[:]

    yield

    root_logger.setLevel(original_level)
    root_logger.handlers[:] = original_handlers


@pytest.fixture
def tty(mocker) -> None:
    """Simulate an interactive terminal (tty) environment."""
    mocker.patch("synctank.logging.sys.stdout.isatty", return_value=True)
    mocker.patch("synctank.logging.sys.stderr.isatty", return_value=True)


@pytest.fixture
def runner() -> CliRunner:
    """Click test runner."""
    return CliRunner()

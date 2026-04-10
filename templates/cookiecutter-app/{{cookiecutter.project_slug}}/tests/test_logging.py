import logging

import pytest

from {{ cookiecutter.package_name }}.logging import (
    InteractiveLogHandler,
    NonInteractiveLogHandler,
    configure_logging,
    get_logger,
)


class TestConfigureLoggingNonInteractive:
    """configure_logging() in non-interactive (non-tty) mode."""

    def test_default_uses_non_interactive_handler(self) -> None:
        configure_logging()
        root = logging.getLogger()
        assert any(isinstance(h, NonInteractiveLogHandler) for h in root.handlers)

    def test_default_level_is_info(self) -> None:
        configure_logging()
        assert logging.getLogger().level == logging.INFO

    def test_debug_level_is_debug(self) -> None:
        configure_logging(debug=True)
        assert logging.getLogger().level == logging.DEBUG

    def test_replaces_existing_handlers(self) -> None:
        logging.getLogger().addHandler(logging.NullHandler())
        configure_logging()
        handlers = logging.getLogger().handlers
        assert len(handlers) == 1
        assert isinstance(handlers[0], NonInteractiveLogHandler)


class TestConfigureLoggingInteractive:
    """configure_logging() in interactive (tty) mode."""

    def test_default_uses_interactive_handler(self, tty) -> None:
        configure_logging()
        root = logging.getLogger()
        assert any(isinstance(h, InteractiveLogHandler) for h in root.handlers)

    def test_default_uses_concise_handler(self, tty) -> None:
        configure_logging()
        handler = next(
            h
            for h in logging.getLogger().handlers
            if isinstance(h, InteractiveLogHandler)
        )
        assert handler._concise is True

    def test_debug_uses_detailed_handler(self, tty) -> None:
        configure_logging(debug=True)
        handler = next(
            h
            for h in logging.getLogger().handlers
            if isinstance(h, InteractiveLogHandler)
        )
        assert handler._concise is False

    def test_debug_level_is_debug(self, tty) -> None:
        configure_logging(debug=True)
        assert logging.getLogger().level == logging.DEBUG


class TestInteractiveLogHandlerFormat:
    """InteractiveLogHandler.format() output in concise mode."""

    @pytest.fixture
    def handler(self) -> InteractiveLogHandler:
        return InteractiveLogHandler(concise=True)

    def _make_record(self, level: int, message: str) -> logging.LogRecord:
        return logging.makeLogRecord({"levelno": level, "msg": message})

    def test_error_wrapped_in_red(self, handler) -> None:
        record = self._make_record(logging.ERROR, "something failed")
        assert handler.format(record) == "[red]something failed[/red]"

    def test_warning_wrapped_in_yellow(self, handler) -> None:
        record = self._make_record(logging.WARNING, "heads up")
        assert handler.format(record) == "[yellow]heads up[/yellow]"

    def test_info_has_no_markup(self, handler) -> None:
        record = self._make_record(logging.INFO, "all good")
        assert handler.format(record) == "all good"


class TestGetLogger:
    """get_logger() returns correctly namespaced loggers."""

    def test_bare_name_gets_package_prefix(self) -> None:
        logger = get_logger("mymodule")
        assert logger.name == "{{ cookiecutter.package_name }}.mymodule"

    def test_already_prefixed_name_is_unchanged(self) -> None:
        logger = get_logger("{{ cookiecutter.package_name }}.mymodule")
        assert logger.name == "{{ cookiecutter.package_name }}.mymodule"

"""Tests for tools restore failures."""

import subprocess
from types import SimpleNamespace

import pytest

from nvim_manager.tools_manager import ToolsManager

_LOCK = '{"gersemi": "0.29.2"}'


@pytest.fixture
def manager():
    return ToolsManager(
        SimpleNamespace(lock_file="mason-lock.json"),
        SimpleNamespace(read_file=lambda _name: _LOCK),
    )


def _fail_install(monkeypatch, error):
    """MasonUpdate and the tool listing succeed; the install raises ``error``."""
    planned = [None, None, error]

    def run(self, command, cwd=None, *, capture_output=False):
        item = planned.pop(0)
        if isinstance(item, BaseException):
            raise item
        return subprocess.CompletedProcess(command, 0, stdout="", stderr="")

    monkeypatch.setattr(ToolsManager, "_run_with_venv", run)


def _process_error(*, stdout="", stderr=""):
    return subprocess.CalledProcessError(1, "nvim", output=stdout, stderr=stderr)


@pytest.mark.parametrize(
    ("error", "expected"),
    [
        (
            _process_error(stdout="gersemi: can't find Rust compiler\n"),
            "Tool restoration failed: MasonBatchInstall failed: "
            "gersemi: can't find Rust compiler",
        ),
        (
            _process_error(stderr="installer blew up\n"),
            "Tool restoration failed: MasonBatchInstall failed: installer blew up",
        ),
        (
            _process_error(stdout="gersemi: failed", stderr="pip: no wheel"),
            "Tool restoration failed: MasonBatchInstall failed: "
            "gersemi: failed\n"
            "pip: no wheel",
        ),
        (
            _process_error(),
            "Tool restoration failed: MasonBatchInstall failed",
        ),
        (
            OSError("nvim missing"),
            "Tool restoration failed: MasonBatchInstall failed",
        ),
        (
            _process_error(
                stdout=(
                    "gersemi: \x1b[1;31merror\x1b[0m: subprocess-exited-with-error "
                    + ("copying parser.rs -> build/lib " * 40)
                    + "error: can't find Rust compiler If you are using pip "
                    "ERROR: Failed building wheel for gersemi"
                ),
            ),
            "Tool restoration failed: MasonBatchInstall failed: "
            "gersemi: can't find Rust compiler",
        ),
        (
            _process_error(
                stdout="gersemi: "
                + ("building " * 80)
                + "Failed building wheel for gersemi",
            ),
            "Tool restoration failed: MasonBatchInstall failed: "
            "gersemi: Failed building wheel for gersemi",
        ),
    ],
    ids=[
        "stdout",
        "stderr",
        "both-streams",
        "no-output",
        "not-a-process",
        "long-log",
        "wheel-only",
    ],
)
def test_restore_failure(manager, monkeypatch, error, expected):
    _fail_install(monkeypatch, error)

    with pytest.raises(RuntimeError) as raised:
        manager.restore()

    assert str(raised.value) == expected


def test_unrecognized_long_log_is_clipped(manager, monkeypatch):
    words = " ".join(f"word{i}" for i in range(80))
    _fail_install(monkeypatch, _process_error(stdout=f"gersemi: {words}"))

    with pytest.raises(RuntimeError) as raised:
        manager.restore()

    message = str(raised.value)
    assert message.startswith(
        "Tool restoration failed: MasonBatchInstall failed: gersemi: word0 ",
    )
    assert message.endswith("...")
    assert "word79" not in message
    assert len(message) < len(words)

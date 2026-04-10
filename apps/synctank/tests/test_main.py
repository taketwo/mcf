from click.testing import CliRunner

from synctank.main import cli


def test_help(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0


def test_debug_flag(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["--debug", "--help"])
    assert result.exit_code == 0

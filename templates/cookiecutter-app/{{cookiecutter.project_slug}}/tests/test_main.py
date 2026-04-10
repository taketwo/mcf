from click.testing import CliRunner

from {{ cookiecutter.package_name }}.main import cli


def test_hello(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["hello"])
    assert result.exit_code == 0


def test_hello_with_name(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["hello", "--name", "TestUser"])
    assert result.exit_code == 0


def test_debug_flag(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["--debug", "hello"])
    assert result.exit_code == 0

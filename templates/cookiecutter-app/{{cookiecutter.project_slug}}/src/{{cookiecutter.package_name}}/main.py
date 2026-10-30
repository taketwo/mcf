from dataclasses import dataclass

import click

from .logging import configure_logging, get_logger

logger = get_logger(__name__)


@dataclass(frozen=True, slots=True)
class Greeter:
    """State that the group's options provide to its subcommands."""

    greeting: str


pass_greeter = click.make_pass_decorator(Greeter)


@click.group()
@click.option("--debug", is_flag=True, default=False, help="Enable debug logging.")
@click.option("--greeting", default="Hello", help="What to greet with.")
@click.pass_context
def cli(ctx: click.Context, *, debug: bool, greeting: str) -> None:
    """Command line interface of {{ cookiecutter.project_name }}."""
    configure_logging(debug=debug)
    ctx.obj = Greeter(greeting)


@cli.command()
@click.option("--name", default="World", help="Who to greet.")
@pass_greeter
def hello(greeter: Greeter, name: str) -> None:
    """Greets the user."""
    logger.debug("Greeting %s", name)
    click.echo(f"{greeter.greeting}, {name}!")


if __name__ == "__main__":
    cli()

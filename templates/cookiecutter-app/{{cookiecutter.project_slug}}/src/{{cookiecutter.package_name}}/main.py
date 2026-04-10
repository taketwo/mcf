import click

from .logging import configure_logging, get_logger

logger = get_logger(__name__)


@click.group()
@click.option("--debug", is_flag=True, default=False, help="Enable debug logging.")
@click.pass_context
def cli(ctx: click.Context, *, debug: bool) -> None:
    """{{ cookiecutter.project_name }} CLI."""
    ctx.ensure_object(dict)
    ctx.obj["debug"] = debug
    configure_logging(debug=debug)


@cli.command()
@click.option("--name", default="World", help="Who to greet.")
def hello(name: str) -> None:
    """Greets the user."""
    logger.info("Hello, %s!", name)


if __name__ == "__main__":
    cli()

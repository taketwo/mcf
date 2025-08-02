"""Command-line interface for nvim-manager."""

from pathlib import Path
from typing import Any

import click
from rich.console import Console
from rich.pretty import Pretty

from .config import Config, ConfigLoadError
from .editor_manager import EditorManager
from .lock_repository import LockRepository
from .logging import configure_logging, DEBUG, get_logger, INFO

console = Console()
logger = get_logger(__name__)


pass_context = click.make_pass_decorator(dict)


@click.group(invoke_without_command=True)
@click.option(
    "--config",
    type=click.Path(exists=True, path_type=Path),
    help="Path to configuration file.",
)
@click.option(
    "--debug",
    is_flag=True,
    help="Enable debug logging.",
)
@click.pass_context
def main(ctx: click.Context, config: Path | None, *, debug: bool = False) -> None:
    """Manage Neovim installations and plugins using lock files."""
    # Configure logging before doing anything else
    configure_logging(level=DEBUG if debug else INFO, concise=not debug)

    try:
        # Load configuration and store in context
        config_obj = Config.from_path_or_default(config)
        logger.debug("Configuration loaded successfully")

        # Create shared objects and store in context
        lock_repo = LockRepository(config_obj.lock_repository)
        editor_manager = EditorManager(config_obj.editor, lock_repo)

        ctx.ensure_object(dict)
        ctx.obj["config"] = config_obj
        ctx.obj["lock_repo"] = lock_repo
        ctx.obj["editor_manager"] = editor_manager

    except ConfigLoadError as e:
        logger.exception("Configuration loading failed")
        console.print(f"[red]Error:[/red] {e}")
        console.print(
            "[yellow]Hint:[/yellow] Create configuration file at "
            "~/.config/nvim-manager/config.toml",
        )
        raise click.Abort from e
    except Exception as e:
        logger.exception("Application error")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


@main.command()
@pass_context
def config(ctx: dict[str, Any]) -> None:
    """Display the loaded config."""
    console.print(
        Pretty(
            ctx["config"],
            expand_all=True,
        ),
    )


@main.command()
@click.option(
    "--editor",
    is_flag=True,
    help="Show editor status.",
)
@click.option(
    "--plugins",
    is_flag=True,
    help="Show plugins status.",
)
@pass_context
def status(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
) -> None:
    """Show current state vs lock files."""
    # If no specific targets are specified, show all
    if not editor and not plugins:
        editor = plugins = True

    try:
        if editor:
            # Use pre-instantiated objects from context
            editor_manager = ctx["editor_manager"]

            # Get status and display
            editor_status = editor_manager.status()
            current = editor_status.get("current_revision") or "none"
            current_date = editor_status.get("current_date")
            lock = editor_status.get("lock_revision") or "none"
            lock_date = editor_status.get("lock_date") or "unknown"
            in_sync = editor_status.get("in_sync", False)

            console.print("\n[bold]Editor Status:[/bold]")
            console.print(f"  Current revision: {current}")
            if current_date:
                console.print(f"  Current date: {current_date}")
            console.print(f"  Lock revision: {lock}")
            console.print(f"  Lock date: {lock_date}")

            if in_sync:
                console.print("  [green]✓ In sync[/green]")
            else:
                console.print("  [yellow]⚠ Out of sync[/yellow]")

        if plugins:
            # TODO: Implement plugin status functionality
            console.print("\n[yellow]Plugin status not yet implemented[/yellow]")

    except Exception as e:
        logger.exception("Failed to get status")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


@main.command()
@click.option(
    "--editor",
    is_flag=True,
    help="Update editor to latest version.",
)
@click.option(
    "--plugins",
    is_flag=True,
    help="Update plugins to latest versions.",
)
@pass_context
def update(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
) -> None:
    """Update to latest versions."""
    # If no specific targets are specified, update all
    if not editor and not plugins:
        editor = plugins = True

    try:
        if editor:
            editor_manager = ctx["editor_manager"]
            editor_manager.update()
        if plugins:
            # TODO: Implement plugin update functionality
            console.print("\n[yellow]Plugin update not yet implemented[/yellow]")

    except Exception as e:
        logger.exception("Failed to update")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


@main.command()
@click.option(
    "--editor",
    is_flag=True,
    help="Commit current editor version to lock file.",
)
@click.option(
    "--plugins",
    is_flag=True,
    help="Commit current plugin versions to lock file.",
)
@pass_context
def commit(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
) -> None:
    """Save current state to lock files."""
    # If no specific targets are specified, commit all
    if not editor and not plugins:
        editor = plugins = True

    try:
        if editor:
            # Use pre-instantiated objects from context
            editor_manager = ctx["editor_manager"]

            editor_manager.commit()
            console.print("[green]✓ Editor version committed to lock file[/green]")

        if plugins:
            # TODO: Implement plugin commit functionality
            logger.warning("Plugin commit functionality not yet implemented")

    except Exception as e:
        logger.exception("Failed to commit")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


@main.command()
@click.option(
    "--editor",
    is_flag=True,
    help="Restore editor to version from lock file.",
)
@click.option(
    "--plugins",
    is_flag=True,
    help="Restore plugins to versions from lock file.",
)
@pass_context
def restore(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
) -> None:
    """Restore to versions specified in lock files."""
    # If no specific targets are specified, restore all
    if not editor and not plugins:
        editor = plugins = True

    try:
        if editor:
            # Use pre-instantiated objects from context
            editor_manager = ctx["editor_manager"]

            console.print("\n[bold]Restoring editor version...[/bold]")
            restored_commit = editor_manager.restore()
            console.print(
                f"[green]✓ Editor restored to commit: {restored_commit}[/green]",
            )

        if plugins:
            # TODO: Implement plugin restore functionality
            console.print("\n[yellow]Plugin restore not yet implemented[/yellow]")

    except Exception as e:
        logger.exception("Failed to restore")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


if __name__ == "__main__":
    main()

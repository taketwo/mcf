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
from .plugin_manager import PluginManager

console = Console()
logger = get_logger(__name__)


def _display_plugin_status(plugin_status: dict[str, Any]) -> None:
    """Display plugin status information with detailed differences."""
    console.print("\n[bold]Plugin Status:[/bold]")

    # Handle error cases
    if "error" in plugin_status:
        console.print(f"  [red]Error: {plugin_status['error']}[/red]")
        return

    console.print(f"  Local plugins: {plugin_status['total_plugins_local']}")
    console.print(f"  Remote plugins: {plugin_status['total_plugins_remote']}")

    if plugin_status["in_sync"]:
        console.print("  [green]✓ In sync[/green]")
    else:
        console.print("  [yellow]⚠ Out of sync[/yellow]")

        differences = plugin_status["differences"]
        if differences:
            console.print(
                f"\n  [bold]Differences ({len(differences)} plugins):[/bold]",
            )
            for diff in differences:
                plugin_name = diff["plugin"]
                status = diff["status"]

                if status == "missing_locally":
                    console.print(
                        f"    [red]- {plugin_name}[/red] (missing locally, remote: {diff['remote_commit'][:8]})",
                    )
                elif status == "missing_remotely":
                    console.print(
                        f"    [yellow]+ {plugin_name}[/yellow] (missing remotely, local: {diff['local_commit'][:8]})",
                    )
                elif status == "different_commits":
                    local_short = diff["local_commit"][:8]
                    remote_short = diff["remote_commit"][:8]
                    console.print(
                        f"    [blue]~ {plugin_name}[/blue] (local: {local_short}, remote: {remote_short})",
                    )


pass_context = click.make_pass_decorator(dict)


@click.group(invoke_without_command=True, chain=True)
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
    """Manage Neovim installations and plugins using lock files.

    Commands can be chained together for workflow efficiency:

        nvim-manager update --editor commit --editor
        nvim-manager restore --plugins update --plugins commit --plugins
    """
    # Configure logging before doing anything else
    configure_logging(level=DEBUG if debug else INFO, concise=not debug)

    try:
        # Load configuration and store in context
        config_obj = Config.from_path_or_default(config)
        logger.debug("Configuration loaded successfully")

        # Create shared objects and store in context
        lock_repo = LockRepository(config_obj.lock_repository)
        editor_manager = EditorManager(config_obj.editor, lock_repo)
        plugin_manager = PluginManager(config_obj.plugins, lock_repo)

        ctx.ensure_object(dict)
        ctx.obj["config"] = config_obj
        ctx.obj["lock_repo"] = lock_repo
        ctx.obj["editor_manager"] = editor_manager
        ctx.obj["plugin_manager"] = plugin_manager

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
            # Use pre-instantiated objects from context
            plugin_manager = ctx["plugin_manager"]

            # Get status and display
            plugin_status = plugin_manager.status()
            _display_plugin_status(plugin_status)

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
            plugin_manager = ctx["plugin_manager"]
            plugin_manager.update()
            console.print("[green]✓ Plugin update completed[/green]")

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
            # Use pre-instantiated objects from context
            plugin_manager = ctx["plugin_manager"]

            plugin_manager.commit()
            console.print("[green]✓ Plugin state committed to lock file[/green]")

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
            # Use pre-instantiated objects from context
            plugin_manager = ctx["plugin_manager"]

            console.print("\n[bold]Restoring plugin versions...[/bold]")
            plugin_manager.restore()
            console.print("[green]✓ Plugins restored from lock file[/green]")

    except Exception as e:
        logger.exception("Failed to restore")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


if __name__ == "__main__":
    main()

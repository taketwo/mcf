"""Command-line interface for nvim-manager."""

from pathlib import Path
from typing import Any

import click
from rich.console import Console
from rich.panel import Panel
from rich.pretty import Pretty
from rich.syntax import Syntax

from .config import Config, ConfigLoadError
from .editor_manager import EditorManager
from .lock_repository import LockRepository
from .logging import configure_logging, DEBUG, get_logger, INFO
from .plugin_manager import PluginManager
from .tools_manager import ToolsManager

console = Console()
logger = get_logger(__name__)


def _display_news_diff(news_diff: str) -> None:
    """Display formatted news diff using Rich.

    Parameters
    ----------
    news_diff : str
        Diff content for runtime/doc/news.txt between versions.

    """
    syntax = Syntax(news_diff, "diff", theme="monokai", line_numbers=False)
    panel = Panel(
        syntax,
        title="[bold blue]Changes in runtime/doc/news.txt[/bold blue]",
        border_style="blue",
        expand=False,
    )
    console.print(panel)


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
        tools_manager = ToolsManager(config_obj.tools, lock_repo)

        ctx.ensure_object(dict)
        ctx.obj["config"] = config_obj
        ctx.obj["lock_repo"] = lock_repo
        ctx.obj["editor_manager"] = editor_manager
        ctx.obj["plugin_manager"] = plugin_manager
        ctx.obj["tools_manager"] = tools_manager

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
@click.option(
    "--show-news/--no-news",
    default=True,
    help="Show/hide news diff for editor updates (default: show).",
)
@pass_context
def update(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
    show_news: bool = True,
) -> None:
    """Update to latest versions."""
    # If no specific targets are specified, update all
    if not editor and not plugins:
        editor = plugins = True

    try:
        if editor:
            editor_manager = ctx["editor_manager"]
            editor_manager.update(
                news_diff_callback=(_display_news_diff if show_news else None),
            )
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
@click.option(
    "--tools",
    is_flag=True,
    help="Commit current tool versions to lock file.",
)
@pass_context
def commit(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
    tools: bool = False,
) -> None:
    """Save current state to lock files."""
    # If no specific targets are specified, commit all
    if not editor and not plugins and not tools:
        editor = plugins = tools = True

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

        if tools:
            # Use pre-instantiated objects from context
            tools_manager = ctx["tools_manager"]

            tools_manager.commit()
            console.print("[green]✓ Tool state committed to lock file[/green]")

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


@main.command()
@pass_context
def sync(ctx: dict[str, Any]) -> None:
    """Execute predefined sync sequence.

    Runs the complete sync workflow:
    1. Update editor to latest version
    2. Commit editor version to lock file
    3. Restore plugins from lock file
    4. Update plugins interactively
    5. Commit plugin state to lock file
    """
    try:
        console.print("[bold]Starting sync workflow...[/bold]")

        # Step 1: Update editor
        console.print("\n[bold]Step 1/5:[/bold] Updating editor to latest version")
        editor_manager = ctx["editor_manager"]
        editor_manager.update()
        console.print("[green]✓ Editor updated[/green]")

        # Step 2: Commit editor
        console.print("\n[bold]Step 2/5:[/bold] Committing editor version")
        editor_manager.commit()
        console.print("[green]✓ Editor version committed to lock file[/green]")

        # Step 3: Restore plugins
        console.print("\n[bold]Step 3/5:[/bold] Restoring plugins from lock file")
        plugin_manager = ctx["plugin_manager"]
        plugin_manager.restore()
        console.print("[green]✓ Plugins restored from lock file[/green]")

        # Step 4: Update plugins
        console.print("\n[bold]Step 4/5:[/bold] Updating plugins interactively")
        plugin_manager.update()
        console.print("[green]✓ Plugin update completed[/green]")

        # Step 5: Commit plugins
        console.print("\n[bold]Step 5/5:[/bold] Committing plugin state")
        plugin_manager.commit()
        console.print("[green]✓ Plugin state committed to lock file[/green]")

        console.print(
            "\n[bold green]🎉 Sync workflow completed successfully![/bold green]",
        )

    except Exception as e:
        logger.exception("Sync workflow failed")
        console.print(f"\n[red]Sync failed:[/red] {e}")
        raise click.Abort from e


if __name__ == "__main__":
    main()

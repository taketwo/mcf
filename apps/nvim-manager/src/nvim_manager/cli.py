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
from .logging import DEBUG, INFO, configure_logging, get_logger
from .plugin_manager import PluginManager
from .tools_manager import ToolsManager
from .utils import LockComparison

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
        title="[bold blue]Changes in runtime/doc/news.txt since lock version[/bold blue]",
        border_style="blue",
        expand=False,
    )
    console.print(panel)


def _display_lock_status(
    status_data: dict[str, Any],
    title: str,
    item_type: str,
) -> None:
    """Display unified current vs lock status for any lock-based manager."""
    console.print(f"\n[bold]{title}:[/bold]")

    # Handle error cases
    if "error" in status_data:
        console.print(f"  [red]Error: {status_data['error']}[/red]")
        return

    # Use standardized field names
    total_current = status_data.get("total_current", 0)
    total_lock = status_data.get("total_lock", 0)

    # Derive plural form for display
    item_name = f"{item_type}s"

    console.print(f"  Current {item_name}: {total_current}")
    console.print(f"  Lock {item_name}: {total_lock}")

    if status_data["in_sync"]:
        console.print("  [green]✓ In sync[/green]")
    else:
        console.print("  [yellow]⚠ Out of sync[/yellow]")
        differences = status_data["differences"]
        if differences:
            console.print(
                f"\n  [bold]Differences ({len(differences)} {item_type}s):[/bold]",
            )

            def format_hash(value: str) -> str:
                """Format hash for display, shortening plugin commit hashes."""
                commit_hash_length = 40
                return (
                    value[:8]
                    if item_type == "plugin"
                    and len(value) == commit_hash_length
                    and all(c in "0123456789abcdef" for c in value.lower())
                    else value
                )

            for diff in differences:
                name = diff.name
                status = diff.status

                if status == LockComparison.Status.MISSING_CURRENTLY:
                    lock_hash = format_hash(diff.lock_value)
                    console.print(
                        f"    [red]- {name}[/red] (missing currently, lock: {lock_hash})",
                    )
                elif status == LockComparison.Status.MISSING_IN_LOCK:
                    current_hash = format_hash(diff.current_value)
                    console.print(
                        f"    [yellow]+ {name}[/yellow] (missing in lock, current: {current_hash})",
                    )
                elif status == LockComparison.Status.DIFFERENT_VALUES:
                    current_hash = format_hash(diff.current_value)
                    lock_hash = format_hash(diff.lock_value)
                    console.print(
                        f"    [blue]~ {name}[/blue] (current: {current_hash}, lock: {lock_hash})",
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
@click.option(
    "--tools",
    is_flag=True,
    help="Show tools status.",
)
@pass_context
def status(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
    tools: bool = False,
) -> None:
    """Show current state vs lock files."""
    # If no specific targets are specified, show all
    if not editor and not plugins and not tools:
        editor = plugins = tools = True

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
            _display_lock_status(plugin_status, "Plugin Status", "plugin")

        if tools:
            # Use pre-instantiated objects from context
            tools_manager = ctx["tools_manager"]

            # Get status and display
            tools_status = tools_manager.status()
            _display_lock_status(tools_status, "Tools Status", "tool")

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
    "--tools",
    is_flag=True,
    help="Update tools to latest versions.",
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
    tools: bool = False,
    show_news: bool = True,
) -> None:
    """Update to latest versions."""
    if not editor and not plugins and not tools:
        editor = plugins = tools = True

    try:
        if editor:
            ctx["editor_manager"].update(
                news_diff_callback=(_display_news_diff if show_news else None),
            )
        if plugins:
            ctx["plugin_manager"].update()
        if tools:
            ctx["tools_manager"].update()
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
    if not editor and not plugins and not tools:
        editor = plugins = tools = True

    try:
        if editor:
            ctx["editor_manager"].commit()
        if plugins:
            ctx["plugin_manager"].commit()
        if tools:
            ctx["tools_manager"].commit()
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
@click.option(
    "--tools",
    is_flag=True,
    help="Restore tools to versions from lock file.",
)
@pass_context
def restore(
    ctx: dict[str, Any],
    *,
    editor: bool = False,
    plugins: bool = False,
    tools: bool = False,
) -> None:
    """Restore to versions specified in lock files."""
    if not editor and not plugins and not tools:
        editor = plugins = tools = True

    try:
        if editor:
            ctx["editor_manager"].restore()
        if plugins:
            ctx["plugin_manager"].restore()
        if tools:
            ctx["tools_manager"].restore()
    except Exception as e:
        logger.exception("Failed to restore")
        console.print(f"[red]Error:[/red] {e}")
        raise click.Abort from e


@main.command()
@click.option(
    "--editor",
    is_flag=True,
    help="Sync editor only.",
)
@click.option(
    "--plugins",
    is_flag=True,
    help="Sync plugins only.",
)
@click.option(
    "--tools",
    is_flag=True,
    help="Sync tools only.",
)
@click.pass_context
def sync(
    ctx: click.Context,
    *,
    editor: bool = False,
    plugins: bool = False,
    tools: bool = False,
) -> None:
    """Execute predefined sync sequence.

    Runs the complete sync workflow:
    1. Update to latest version
    2. Commit version to lock file
    3. Restore from lock file (plugins/tools only)
    4. Update interactively
    5. Commit state to lock file
    """
    # If no specific targets are specified, sync all
    if not editor and not plugins and not tools:
        editor = plugins = tools = True

    console.print("[bold]Starting sync workflow...[/bold]")

    sync_steps: list[tuple[click.Command, dict[str, Any]]] = []
    if editor:
        sync_steps.extend(
            [
                (update, {"editor": True, "plugins": False, "tools": False}),
                (commit, {"editor": True, "plugins": False, "tools": False}),
            ],
        )
    if plugins:
        sync_steps.extend(
            [
                (restore, {"editor": False, "plugins": True, "tools": False}),
                (update, {"editor": False, "plugins": True, "tools": False}),
                (commit, {"editor": False, "plugins": True, "tools": False}),
            ],
        )
    if tools:
        sync_steps.extend(
            [
                (restore, {"editor": False, "plugins": False, "tools": True}),
                (update, {"editor": False, "plugins": False, "tools": True}),
                (commit, {"editor": False, "plugins": False, "tools": True}),
            ],
        )

    try:
        for i, (cmd, kwargs) in enumerate(sync_steps, 1):
            console.print(f"\n[bold]Step {i}/{len(sync_steps)}:[/bold]")
            ctx.invoke(cmd, **kwargs)
        console.print(
            "\n[bold green]🎉 Sync workflow completed successfully![/bold green]",
        )
    except Exception as e:
        logger.exception("Sync workflow failed")
        console.print(f"\n[red]Sync failed:[/red] {e}")
        raise click.Abort from e


if __name__ == "__main__":
    main()

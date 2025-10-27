import json
import sys
from collections.abc import Callable

import click

from ccu.logging import configure_logging, get_logger
from ccu.project import get_project

logger = get_logger(__name__)


@click.group()
@click.option("--debug", is_flag=True, envvar="CCU_DEBUG", help="Enable debug logging")
def main(*, debug: bool = False) -> None:
    """ClaudeCode Utilities CLI."""
    configure_logging(debug=debug)


@main.command()
@click.argument("hook_names", nargs=-1, required=False)
def hook(hook_names: tuple[str, ...] | None = None) -> None:
    """Execute one or more hook commands.

    If no arguments provided, list available hooks.
    """
    try:
        # Import hooks module only when needed
        from . import hooks  # noqa: PLC0415

        # If no hook names provided, list available hooks
        if not hook_names:
            available_hooks = hooks.list_available_hooks()
            print("Available hooks:")
            for hook_name in available_hooks:
                print(f"  {hook_name}")
            return

        try:
            input_data = json.load(sys.stdin)
        except (json.JSONDecodeError, KeyError) as e:
            print(f"Error parsing hook input from stdin: {e}", file=sys.stderr)
            sys.exit(2)

        project = get_project()
        try:
            hook_instances = [
                hooks.get_hook(hook_name, project) for hook_name in hook_names
            ]
        except hooks.UnknownHookError as e:
            print(str(e), file=sys.stderr)
            sys.exit(2)

        combined_exit_code = 0
        for hook_instance in hook_instances:
            result = hook_instance.execute(input_data)
            logger.debug(
                "Hook %s executed with exit code %d for file %s",
                hook_instance.__class__.__name__,
                result.exit_code,
                result.file_path,
            )
            if result.output:
                print(
                    f"\n{result.tool_name} output:\n{result.output}",
                    file=(sys.stderr if result.exit_code else sys.stdout),
                )
            if result.exit_code == hooks.ExitCode.BLOCKING_ERROR:
                combined_exit_code = hooks.ExitCode.BLOCKING_ERROR
            elif (
                result.exit_code != 0
                and combined_exit_code != hooks.ExitCode.BLOCKING_ERROR
            ):
                combined_exit_code = hooks.ExitCode.NON_BLOCKING_ERROR

        logger.debug(
            "All hooks executed, combined exit code: %d",
            combined_exit_code,
        )
        sys.exit(combined_exit_code)

    except Exception:
        logger.exception("Unexpected error in hook execution")
        print("Internal error during hook execution", file=sys.stderr)
        sys.exit(2)


@main.command()
@click.argument("hook_name")
@click.argument("file_path")
def debug_hook(hook_name: str, file_path: str) -> None:
    """Debug hook execution.

    Automatically enables debug logging and simulates Claude Code hook execution
    for the specified file.
    """
    configure_logging(debug=True)
    try:
        # Import hooks module only when needed
        from . import hooks  # noqa: PLC0415

        try:
            project = get_project()
            hook_instance = hooks.get_hook(hook_name, project)
        except hooks.UnknownHookError as e:
            print(str(e), file=sys.stderr)
            sys.exit(2)

        result = hook_instance.execute({"tool_input": {"file_path": file_path}})
        logger.debug(
            "Hook %s executed with exit code %d for file %s",
            hook_instance.__class__.__name__,
            result.exit_code,
            result.file_path,
        )
        if result.output:
            print(
                f"\n{result.tool_name} output:\n{result.output}",
                file=(sys.stderr if result.exit_code else sys.stdout),
            )
        sys.exit(result.exit_code)

    except Exception:
        logger.exception("Unexpected error in debug hook execution")
        sys.exit(2)


@main.command()
@click.option(
    "--dry-run",
    is_flag=True,
    help="Show what would be done without executing",
)
def plugins(*, dry_run: bool = False) -> None:
    """Align local plugin configuration with reference.

    Ensures configured marketplaces and installed plugins match the reference
    configuration by adding any missing items. Reports extra items but does not
    remove them.
    """
    reference_config = {
        "marketplaces": [
            "obra/superpowers-marketplace",
        ],
        "plugins": [
            "superpowers@superpowers-marketplace",
        ],
    }

    try:
        from . import plugins  # noqa: PLC0415

        current_marketplaces = plugins.get_marketplaces()
        current_plugins = plugins.get_plugins()

        errors = []

        def process_items(
            title: str,
            reference_items: list[str],
            current_items: set[str],
            add_func: Callable[[str], tuple[bool, str]],
        ) -> None:
            item_type = title.lower().rstrip("s")
            print(f"{title}:")
            for item in reference_items:
                if item in current_items:
                    print(f"  ✓ {item}")
                elif dry_run:
                    logger.info("[DRY RUN] Would add %s: %s", item_type, item)
                    print(f"  ⊕ {item}")
                else:
                    logger.info("Adding %s: %s", item_type, item)
                    success, error = add_func(item)
                    if success:
                        print(f"  ✚ {item}")
                    else:
                        error_msg = f"Failed to add {item_type} {item}: {error}"
                        logger.error(error_msg)
                        errors.append(error_msg)
                        print(f"  ✗ {item}", file=sys.stderr)
            for item in current_items - set(reference_items):
                print(f"  ⚠ {item}")

        process_items(
            "Marketplaces",
            reference_config["marketplaces"],
            current_marketplaces,
            plugins.add_marketplace,
        )
        process_items(
            "Plugins",
            reference_config["plugins"],
            current_plugins,
            plugins.add_plugin,
        )

        if errors:
            print("\nSome operations failed", file=sys.stderr)
            sys.exit(1)

    except Exception:
        logger.exception("Unexpected error in plugins command")
        print("Internal error during plugin alignment", file=sys.stderr)
        sys.exit(2)


if __name__ == "__main__":
    main()

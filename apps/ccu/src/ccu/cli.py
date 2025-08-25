import json
import sys

import click

from ccu.logging import configure_logging, get_logger

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

        try:
            hook_instances = [hooks.get_hook(hook_name) for hook_name in hook_names]
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


if __name__ == "__main__":
    main()

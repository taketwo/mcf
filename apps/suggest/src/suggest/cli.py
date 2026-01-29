"""Command-line interface for the suggest tool.

This module provides the main entry point and argument parsing
for the suggest command-line tool.
"""

import argparse
import asyncio
import time
from collections.abc import Callable
from typing import Any, Protocol

import pyperclip
from humanize import naturalsize
from rich.console import Console
from rich.status import Status

from .llm_client import LLMClient
from .logging import configure_logging, get_logger
from .mock_llm_client import MockLLMClient
from .user_interface import (
    display_explanation,
    display_generated_command,
    get_revision_request,
    get_user_request,
    show_menu,
)

logger = get_logger(__name__)


class LLMClientProtocol(Protocol):
    """Protocol for LLM client implementations."""

    async def request_command(
        self,
        request: str,
        on_progress: Callable[[int], None] | None = None,
    ) -> dict[str, Any]:
        """Generate or revise a shell command from a natural language request."""
        ...

    async def explain_command(
        self,
        on_progress: Callable[[int], None] | None = None,
    ) -> dict[str, Any]:
        """Explain the last generated command."""
        ...


def make_progress_callback(
    status: Status,
    base_message: str,
    throttle_ms: float = 100,
) -> Callable[[int], None]:
    """Create a throttled progress callback for status updates.

    Parameters
    ----------
    status : Status
        Rich Status object to update
    base_message : str
        Base status message (e.g., "Generating command")
    throttle_ms : float
        Minimum milliseconds between updates (default: 100)

    Returns
    -------
    Callable[[int], None]
        Progress callback function that takes bytes received

    """
    last_update = 0.0

    def callback(bytes_received: int) -> None:
        nonlocal last_update
        now = time.monotonic()
        if now - last_update >= throttle_ms / 1000:
            size_str = naturalsize(bytes_received)
            status.update(f"{base_message}... [dim](↓ {size_str})[/dim]")
            last_update = now

    return callback


def parse_arguments() -> argparse.Namespace:
    """Parse command line arguments.

    Returns
    -------
    argparse.Namespace
        Parsed command line arguments.

    """
    parser = argparse.ArgumentParser(
        description="Suggest - Convert natural language to CLI commands using local LLMs",
    )

    parser.add_argument(
        "request",
        nargs="*",
        help="Natural language request for command generation",
    )

    parser.add_argument(
        "--model",
        "-m",
        default="openrouter/upstage/solar-pro-3:free",
        help="LLM model to use",
    )

    parser.add_argument(
        "--mock",
        action="store_true",
        help="Use mock LLM client for UX testing (no actual LLM calls)",
    )

    return parser.parse_args()


async def main_loop(
    console: Console,
    llm_client: LLMClientProtocol,
    initial_request: str | None,
) -> None:
    """Run main interactive loop for command generation.

    Parameters
    ----------
    console : Console
        Rich console instance for user interface.
    llm_client : LLMClientProtocol
        LLM client for command generation.
    initial_request : str | None
        Initial request from command line arguments.

    """
    request = initial_request

    while True:
        # Acquire request
        if not request:
            request = get_user_request(console)
            if not request:
                return

        logger.info("Processing request: %s", request)

        # Generate command
        msg = "[bold blue]Generating command[/bold blue]"
        with console.status(f"{msg}...") as status:
            result = await llm_client.request_command(
                request,
                on_progress=make_progress_callback(status, msg),
            )

        logger.info(
            "Generated command: %s (confidence: %s)",
            result["command"],
            result.get("confidence", "N/A"),
        )

        console.print()
        display_generated_command(console, result)

        # Menu interaction loop
        while True:
            choice = show_menu(console)
            logger.info("User chose menu option: %s", choice)

            if choice == "copy":
                try:
                    pyperclip.copy(result["command"])
                    logger.info("Command copied to clipboard successfully")
                except pyperclip.PyperclipException as e:
                    console.print(
                        "[bold yellow]⚠️  Could not copy to clipboard.[/bold yellow]",
                    )
                    logger.warning("Failed to copy to clipboard: %s", e)
                return

            if choice == "revise":
                request = get_revision_request(console)
                if request:
                    logger.info("User requested revision: %s", request)
                    break
                # If empty revision request, stay in menu

            elif choice == "explain":
                logger.info("Generating explanation for command")
                msg = "[bold blue]Generating explanation[/bold blue]"
                with console.status(f"{msg}...") as status:
                    explanation = await llm_client.explain_command(
                        on_progress=make_progress_callback(status, msg),
                    )
                console.print()
                display_explanation(console, explanation)

            elif choice == "exit":
                return


def main() -> None:
    """Run the suggest command-line tool."""
    # Parse arguments
    args = parse_arguments()

    # Configure logging
    configure_logging()
    logger.info("Starting suggest CLI")
    logger.info("Arguments: model=%s, mock=%s", args.model, args.mock)

    # Create console
    console = Console()

    # Create LLM client (mock or real)
    if args.mock:
        logger.info("Using mock LLM client")
        llm_client: LLMClientProtocol = MockLLMClient()
    else:
        logger.info("Using real LLM client with model: %s", args.model)
        llm_client = LLMClient(model_name=args.model)

    # Combine request arguments into single string
    request = " ".join(args.request) if args.request else None
    if request:
        logger.info("Initial request from command line: %s", request)

    # Run main loop
    try:
        asyncio.run(main_loop(console, llm_client, request))
    except KeyboardInterrupt:
        logger.info("Interrupted by user (Ctrl+C)")
        console.print("\n[dim]Interrupted[/dim]")
    except Exception as e:
        logger.exception("Unexpected error in main loop")
        console.print(f"\n[bold red]Error: {e}[/bold red]")
        raise


if __name__ == "__main__":
    main()

"""User interface module for the suggest tool.

This module handles all user interaction using Rich for
beautiful terminal rendering.
"""

import sys
import termios
import tty
from typing import Any

from rich.console import Console
from rich.panel import Panel
from rich.prompt import Prompt
from rich.table import Table


def display_generated_command(console: Console, result: dict[str, Any]) -> None:
    """Display the generated command with all relevant information.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.
    result : Dict[str, Any]
        Command generation result containing command and confidence.

    """
    command = result["command"]
    confidence = result["confidence"]

    console.print(f"Command: [yellow]{command}[/yellow]", highlight=False)
    console.print(f"Confidence: {confidence:.1%}", highlight=False)


def display_explanation(
    console: Console,
    explanation: dict[str, Any],
) -> None:
    """Display the command explanation with breakdown.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.
    explanation : dict[str, Any]
        Explanation result containing hierarchical breakdown.

    """
    breakdown = explanation.get("breakdown", [])

    # Calculate max width for first column
    max_width = 0
    for cmd_info in breakdown:
        command = cmd_info.get("command", "")
        max_width = max(max_width, len(command))
        parts = cmd_info.get("parts", [])
        for part in parts:
            component = part.get("component", "")
            # Tree guide is "└─ " or "├─ " (3 characters)
            max_width = max(max_width, len(component) + 3)

    # Set column width to max(16, max_width + 2)
    col_width = max(16, max_width + 2)

    # Create single grid table for all commands
    table = Table.grid(padding=(0, 1))
    table.add_column(width=col_width)  # Tree guide + component
    table.add_column()  # Description

    # Add all commands and their parts
    for cmd_info in breakdown:
        command = cmd_info.get("command", "")
        description = cmd_info.get("description", "")
        parts = cmd_info.get("parts", [])

        # Add command as root
        table.add_row(f"[bold yellow]{command}[/bold yellow]", description)

        # Add parts with tree guides
        for i, part in enumerate(parts):
            component = part.get("component", "")
            part_desc = part.get("description", "")
            is_last = i == len(parts) - 1
            guide = "└─" if is_last else "├─"
            table.add_row(
                f"[bright_black]{guide}[/bright_black] [cyan]{component}[/cyan]",
                part_desc,
            )

    console.print(table)


def show_menu(console: Console) -> str:
    """Display interactive menu and get user choice.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.

    Returns
    -------
    str
        User's choice: 'copy', 'revise', 'explain', or 'exit'.

    """
    console.print()
    console.print("[bold blue]What would you like to do?[/bold blue]")
    console.print()

    # Create menu table
    menu_table = Table.grid(padding=(0, 1))
    menu_table.add_column(style="bold")
    menu_table.add_column()

    menu_table.add_row("1", "Copy command to clipboard")
    menu_table.add_row("2", "Revise command")
    menu_table.add_row("3", "Explain command")
    menu_table.add_row("4", "Exit")

    console.print(menu_table)
    console.print()

    # Set terminal to raw mode for single character reading
    fd = sys.stdin.fileno()
    old_settings = termios.tcgetattr(fd)

    try:
        tty.setraw(sys.stdin.fileno())
        while True:
            ch = sys.stdin.read(1)

            if ch == "1":
                # Clear menu (move cursor up 8 lines and clear from cursor to end)
                sys.stdout.write("\033[8A\033[0J")
                sys.stdout.flush()
                return "copy"
            if ch == "2":
                # Clear menu (move cursor up 8 lines and clear from cursor to end)
                sys.stdout.write("\033[8A\033[0J")
                sys.stdout.flush()
                return "revise"
            if ch == "3":
                # Clear menu (move cursor up 8 lines and clear from cursor to end)
                sys.stdout.write("\033[8A\033[0J")
                sys.stdout.flush()
                return "explain"
            if ch == "4":
                # Clear menu (move cursor up 8 lines and clear from cursor to end)
                sys.stdout.write("\033[8A\033[0J")
                sys.stdout.flush()
                return "exit"
            # Ignore all other keys silently
    finally:
        # Restore terminal settings
        termios.tcsetattr(fd, termios.TCSADRAIN, old_settings)


def get_user_request(console: Console) -> str:
    """Get natural language request from user.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.

    Returns
    -------
    str
        User's natural language request.

    """
    console.print()
    request = Prompt.ask(
        "[bold blue]What would you like to do?[/bold blue]",
    )

    if not request.strip():
        console.print("[red]No request provided.[/red]")
        return ""

    return request


def get_revision_request(console: Console) -> str:
    """Get revision request from user.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.

    Returns
    -------
    str
        User's revision request.

    """
    console.print()
    request = Prompt.ask(
        "[bold blue]How would you like to revise the command?[/bold blue]",
    )

    if not request.strip():
        console.print("[red]No revision provided.[/red]")
        return ""

    return request


def show_welcome_message(console: Console) -> None:
    """Display welcome message with tool information.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.

    """
    # Welcome message removed for cleaner UI


def show_goodbye_message(console: Console) -> None:
    """Display goodbye message.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.

    """
    # Goodbye message removed for cleaner exit


def show_error_message(console: Console, error: str) -> None:
    """Display error message.

    Parameters
    ----------
    console : Console
        Rich console instance for rendering.
    error : str
        Error message to display.

    """
    console.print()
    error_panel = Panel(
        f"[red]❌ Error: {error}[/red]",
        border_style="red",
        padding=(1, 2),
    )
    console.print(error_panel)

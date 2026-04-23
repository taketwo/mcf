from __future__ import annotations

from itertools import groupby
from typing import TYPE_CHECKING

from rich.console import Console, Group
from rich.table import Table
from rich.text import Text

if TYPE_CHECKING:
    from pathlib import Path

    from rich.console import RenderableType

    from .lint import LintViolation
    from .notes import Note, ParseError
    from .search import SearchResult


def render_notes_table(
    notes: list[Note], errors: list[ParseError], *, notes_root: Path
) -> RenderableType:
    """Render notes grouped by subdirectory with Rich table output."""
    renderables: list[RenderableType] = []

    def group_key(note: Note) -> str:
        try:
            rel = note.path.parent.relative_to(notes_root)
            return str(rel) if str(rel) != "." else ""
        except ValueError:
            return str(note.path.parent)

    sorted_notes = sorted(notes, key=group_key)

    for subdir, group_notes in groupby(sorted_notes, key=group_key):
        label = f"notes/{subdir}" if subdir else "notes/"
        renderables.append(Text(label, style="bold cyan"))

        table = Table(show_header=True, header_style="bold", box=None, padding=(0, 1))
        table.add_column("Name")
        table.add_column("Kind")
        table.add_column("Status")
        table.add_column("Date", style="dim")
        table.add_column("File", style="dim")

        for note in group_notes:
            table.add_row(
                note.meta.name,
                note.meta.kind.value,
                note.meta.status.value,
                note.meta.date.isoformat(),
                note.path.name,
            )

        renderables.append(table)

    renderables.extend(Text(f"  error: {error}", style="red") for error in errors)

    if not renderables:
        renderables.append(Text("No notes found.", style="dim"))

    return Group(*renderables)


def render_search_results(results: list[SearchResult]) -> RenderableType:
    """Render search results as a Rich table."""
    if not results:
        return Text("No results found.", style="dim")

    table = Table(show_header=True, header_style="bold", box=None, padding=(0, 1))
    table.add_column("Score", justify="right", style="dim")
    table.add_column("File")
    table.add_column("Name")
    table.add_column("Excerpt", style="dim")

    for result in results:
        excerpt = result.excerpt or ""
        if result.line_number is not None:
            excerpt = f":{result.line_number}: {excerpt}"
        table.add_row(
            str(result.score),
            result.note.path.name,
            result.note.meta.name,
            excerpt,
        )

    return table


def render_lint_violations(violations: list[LintViolation]) -> RenderableType:
    """Render lint violations as Rich text output."""
    if not violations:
        return Text("No violations found.", style="green")

    return Group(
        *[Text(f"{v.path.name}: {v.message}", style="yellow") for v in violations]
    )


def render_status(root: Path | None, synctank_dir: Path) -> RenderableType:
    """Render the workspace setup status."""
    if root is None:
        return Text("Not linked to synctank.", style="dim")

    lines: list[RenderableType] = []
    for item in sorted(root.iterdir()):
        if item.is_symlink():
            target = item.resolve()
            try:
                target.relative_to(synctank_dir)
                project = target.relative_to(synctank_dir).parts[0]
                lines.append(Text(f"Project: {project}"))
                lines.append(Text(f"Symlink: {item.name} → {target}", style="dim"))
            except ValueError:
                continue

    if not lines:
        return Text("Not linked to synctank.", style="dim")

    return Group(*lines)


def print_renderable(renderable: RenderableType) -> None:
    """Print a Rich renderable to stdout."""
    console = Console()
    console.print(renderable)

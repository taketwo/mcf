from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
from dataclasses import dataclass, field
from datetime import date
from pathlib import Path

import click
import pyperclip

from . import migrate as migrate_mod
from .lint import lint_path
from .logging import configure_logging
from .notes import (
    Frontmatter,
    ParseError,
    enumerate_notes,
    load_note,
    update_note,
    write_note,
)
from .rendering import (
    print_renderable,
    render_lint_violations,
    render_notes_table,
    render_search_results,
    render_status,
)
from .schema import Kind, Status
from .search import SearchResult, search_notes
from .workspace import (
    find_notes_symlink,
    find_workspace_root,
    get_synctank_dir,
    resolve_notes_root,
)


def human_only(cmd: click.Command) -> click.Command:
    """Mark a command as human-only (excluded from agent-facing help section)."""
    cmd.human_only = True  # type: ignore[attr-defined]  # ty: ignore[unresolved-attribute]
    return cmd


class _GroupedHelp(click.Group):
    """Click Group that renders commands in two labelled sections."""

    def format_commands(
        self, ctx: click.Context, formatter: click.HelpFormatter
    ) -> None:
        all_commands = list(self.list_commands(ctx))
        limit = formatter.width - 6 - max((len(n) for n in all_commands), default=0)

        agent_rows: list[tuple[str, str]] = []
        human_rows: list[tuple[str, str]] = []

        for name in all_commands:
            cmd = self.get_command(ctx, name)
            if cmd is None or cmd.hidden:
                continue
            row = (name, cmd.get_short_help_str(limit))
            if getattr(cmd, "human_only", False):
                human_rows.append(row)
            else:
                agent_rows.append(row)

        if agent_rows:
            with formatter.section("Commands (for agents)"):
                formatter.write_dl(agent_rows)
        if human_rows:
            with formatter.section("Commands (for humans)"):
                formatter.write_dl(human_rows)


_KIND_STATUS_EPILOG = (
    "\b\nKind values:\n"
    + "\n".join(f"  {k.value:<18}  {k.description}" for k in Kind)
    + "\n\n\b\nStatus values:\n"
    + "\n".join(f"  {s.value:<12}  {s.description}" for s in Status)
)


@click.group(cls=_GroupedHelp)
@click.option("--debug", is_flag=True, default=False, help="Enable debug logging.")
@click.pass_context
def cli(ctx: click.Context, *, debug: bool) -> None:
    """Manage synced ephemeral notes directories across machines."""
    ctx.ensure_object(dict)
    ctx.obj["debug"] = debug
    configure_logging(debug=debug)


@human_only
@cli.command()
@click.option(
    "--link-name", default="notes", show_default=True, help="Local symlink name."
)
def setup(link_name: str) -> None:
    """Set up synctank for the current project.

    Creates a project directory in the Synctank store, creates a local symlink,
    and adds the symlink to .git/info/exclude.

    \b
    Note: Agents should not run this command. The choice of where to run setup
    is sensitive and belongs to the human operator.
    """  # noqa: D301
    cwd = Path.cwd()
    synctank_dir = get_synctank_dir()

    symlink_path = cwd / link_name
    if symlink_path.is_symlink():
        target = symlink_path.resolve()
        try:
            target.relative_to(synctank_dir)
            click.echo(f"Already set up: {link_name} -> {target}")
        except ValueError:
            raise click.ClickException(
                f"{link_name} exists but points outside the Synctank directory"
            ) from None
        return

    project_name = cwd.name
    project_dir = synctank_dir / project_name
    project_dir.mkdir(parents=True, exist_ok=True)
    click.echo(f"Created {project_dir}/")

    symlink_path.symlink_to(project_dir)
    click.echo(f"Symlinked ./{link_name} -> {project_dir}/")

    _ensure_git_excluded(cwd, link_name)


@cli.command(epilog=_KIND_STATUS_EPILOG)
@click.argument("name")
@click.option(
    "--kind",
    type=click.Choice([k.value for k in Kind]),
    required=True,
    help="Document kind (see vocabulary below).",
)
@click.option(
    "--status",
    type=click.Choice([s.value for s in Status]),
    required=True,
    help="Document status (see vocabulary below).",
)
@click.option(
    "--subdir",
    default=None,
    help="Subdirectory to place note that belongs to a distinct subproject or workstream.",
)
@click.option(
    "--related",
    multiple=True,
    help="Filename of a related note, e.g. 001-foo-design.md. Repeatable.",
)
@click.option(
    "--json",
    "as_json",
    is_flag=True,
    default=False,
    help="Output metadata of created note as JSON.",
)
def create(  # noqa: PLR0913
    name: str,
    kind: str,
    status: str,
    subdir: str | None,
    related: tuple[str, ...],
    *,
    as_json: bool,
) -> None:
    """Create a new note.

    Each note requires a kind and a status from fixed vocabularies — see the descriptions at the end of this help to pick the right values. Both are required so that notes are meaningfully categorized from the start.

    A note can be created as a stub (frontmatter and heading only) or with body content passed via stdin. The tool assembles the complete file — frontmatter, heading, and body — so you never write frontmatter manually.

    \b
    Rules:
      - Sentence case everywhere: names, headings, body text.
      - Name is subject only, no kind: "Decoder refactor", not "Decoder refactor design".
      - No H1 heading in body — generated from name and kind.
      - No line wrapping — one paragraph per line, no matter how long.

    Notes are created within current project's notes root by default, or within a specified subdirectory. The latter is useful for grouping notes that belong to a distinct subproject or workstream. Use only when a subset of work is large enough to warrant its own namespace.

    Prints the absolute path of the created file. With --json, outputs the note metadata as a JSON object instead.

    \b
    Example:
      synctank create "Decoder refactor" --kind design --status draft << 'EOF'
      ## Overview
      Body content here.
      EOF
    """  # noqa: D301
    cwd = Path.cwd()
    synctank_dir = get_synctank_dir()
    notes_root = resolve_notes_root(cwd, synctank_dir)

    body = ""
    if not sys.stdin.isatty():
        body = sys.stdin.read()

    fm = Frontmatter(
        name=name,
        kind=Kind(kind),
        status=Status(status),
        date=date.today(),
        related=list(related),
    )
    note = write_note(notes_root / subdir if subdir else notes_root, fm, body)

    if as_json:
        click.echo(json.dumps(note.to_dict()))
    else:
        click.echo(str(note.path))


@cli.command(epilog=_KIND_STATUS_EPILOG)
@click.argument("path", type=click.Path(exists=True, dir_okay=False, path_type=Path))
@click.option(
    "--name",
    default=None,
    help="New document name.",
)
@click.option(
    "--kind",
    type=click.Choice([k.value for k in Kind]),
    default=None,
    help="New document kind (see vocabulary below).",
)
@click.option(
    "--status",
    type=click.Choice([s.value for s in Status]),
    default=None,
    help="New document status (see vocabulary below).",
)
@click.option(
    "--related",
    multiple=True,
    default=None,
    help="Replace related list with these filenames. Repeatable.",
)
@click.option(
    "--json",
    "as_json",
    is_flag=True,
    default=False,
    help="Output metadata of updated note as JSON.",
)
@click.pass_context
def update(  # noqa: PLR0913
    ctx: click.Context,
    path: Path,
    name: str | None,
    kind: str | None,
    status: str | None,
    related: tuple[str, ...],
    *,
    as_json: bool,
) -> None:
    """Update frontmatter fields of an existing note.

    All options are optional — only provided fields are changed. The file is
    rewritten with standardized frontmatter, and renamed if the name or kind
    change affects the filename.

    Prints the (possibly new) absolute path of the updated file. With --json,
    outputs the note metadata as a JSON object instead.

    \b
    Rules:
      - Sentence case everywhere: names, headings, body text.
      - Name is subject only, no kind: "Decoder refactor", not "Decoder refactor design".

    \b
    Example:
      synctank update notes/001-foo-design.md --status complete
      synctank update notes/001-foo-design.md --name "New name" --kind report
    """  # noqa: D301
    related_provided = (
        ctx.get_parameter_source("related") != click.core.ParameterSource.DEFAULT
    )

    if name is None and kind is None and status is None and not related_provided:
        raise click.UsageError("Nothing to update. Provide at least one option.")

    note = update_note(
        path,
        name=name,
        kind=Kind(kind) if kind else None,
        status=Status(status) if status else None,
        related=list(related) if related_provided else None,
    )

    if as_json:
        click.echo(json.dumps(note.to_dict()))
    else:
        click.echo(str(note.path))


@cli.command("list")
@click.argument("subdir", required=False, default=None)
@click.option(
    "--json", "as_json", is_flag=True, default=False, help="Output as JSON array."
)
def list_notes(subdir: str | None, *, as_json: bool) -> None:
    """List notes (recursively from notes root or SUBDIR)."""
    cwd = Path.cwd()
    synctank_dir = get_synctank_dir()
    notes_root = resolve_notes_root(cwd, synctank_dir)

    search_root = notes_root / subdir if subdir else notes_root

    notes = []
    errors: list[ParseError] = []
    for p in enumerate_notes(search_root):
        try:
            notes.append(load_note(p))
        except ParseError as e:
            errors.append(e)

    if as_json:
        click.echo(json.dumps([n.to_dict() for n in notes]))
    else:
        print_renderable(render_notes_table(notes, errors, notes_root=notes_root))


@human_only
@cli.command()
@click.option(
    "--json", "as_json", is_flag=True, default=False, help="Output status as JSON."
)
def status(*, as_json: bool) -> None:
    """Show setup health of the current directory."""
    cwd = Path.cwd()
    synctank_dir = get_synctank_dir()
    root = find_workspace_root(cwd, synctank_dir)

    if as_json:
        if root is None:
            click.echo(json.dumps({"linked": False}))
            return
        notes_root = find_notes_symlink(root, synctank_dir)
        click.echo(
            json.dumps(
                {
                    "linked": notes_root is not None,
                    "workspace_root": str(root),
                    "notes_root": str(notes_root) if notes_root else None,
                }
            )
        )
    else:
        print_renderable(render_status(root, synctank_dir))


@cli.command()
@click.argument("target", required=False, default=None)
@click.option(
    "--detailed", is_flag=True, default=False, help="Show per-violation detail."
)
def lint(target: str | None, *, detailed: bool) -> None:
    """Validate notes in TARGET (file or directory, default: notes root)."""
    cwd = Path.cwd()
    synctank_dir = get_synctank_dir()

    notes_root = resolve_notes_root(cwd, synctank_dir)
    path = Path(target) if target else notes_root

    violations = lint_path(path)
    print_renderable(
        render_lint_violations(violations, notes_root=notes_root, detailed=detailed)
    )


@cli.command()
@click.argument("query")
@click.option("--everywhere", is_flag=True, default=False, help="Search all projects.")
@click.option(
    "--names-only", is_flag=True, default=False, help="Search filenames only."
)
@click.option(
    "--json", "as_json", is_flag=True, default=False, help="Output as JSON array."
)
def search(query: str, *, everywhere: bool, names_only: bool, as_json: bool) -> None:
    """Fuzzy search notes. Searches current project by default."""
    cwd = Path.cwd()
    synctank_dir = get_synctank_dir()

    roots: list[Path] = []
    if everywhere:
        roots = _all_project_roots(synctank_dir)
    else:
        root = find_workspace_root(cwd, synctank_dir)
        if root is not None:
            notes_root = find_notes_symlink(root, synctank_dir)
            if notes_root is not None:
                roots = [notes_root]
        if not roots:
            roots = _all_project_roots(synctank_dir)

    results = search_notes(query, roots, names_only=names_only)

    if as_json:
        click.echo(
            json.dumps(
                [
                    {
                        "score": r.score,
                        "note": r.note.to_dict(),
                        "excerpt": r.excerpt,
                        "line_number": r.line_number,
                    }
                    for r in results
                ]
            )
        )
    elif sys.stdout.isatty() and shutil.which("fzf"):
        sys.exit(_run_fzf_search(results))
    else:
        print_renderable(render_search_results(results))


@human_only
@cli.command()
@click.argument(
    "path",
    required=False,
    default=None,
    type=click.Path(exists=True, file_okay=False, path_type=Path),
)
@click.option(
    "--dry-run",
    is_flag=True,
    default=False,
    help="Print planned changes without modifying files.",
)
def migrate(path: Path | None, *, dry_run: bool) -> None:
    """Migrate pre-existing notes into synctank conventions.

    Renames files to the NNN-slug-kind.md scheme and rewrites frontmatter.
    Run with --dry-run first to review the plan before touching files.
    """
    if path is None:
        cwd = Path.cwd()
        synctank_dir = get_synctank_dir()
        path = resolve_notes_root(cwd, synctank_dir)

    plans = migrate_mod.plan_migration(path)

    stats = _MigrateStats()
    for directory, plan in plans.items():
        _print_migrate_plan(path, directory, plan, stats, dry_run=dry_run)

    mode = " (dry run)" if dry_run else ""
    click.echo(
        f"\n{stats.renamed} renamed, {stats.rewritten} rewritten, "
        f"{stats.skipped} skipped, {stats.warned} warnings, {stats.errors} errors{mode}"
    )


@dataclass
class _MigrateStats:
    renamed: int = field(default=0)
    rewritten: int = field(default=0)
    skipped: int = field(default=0)
    warned: int = field(default=0)
    errors: int = field(default=0)


def _print_migrate_plan(
    root: Path,
    directory: Path,
    plan: migrate_mod.MigratePlan,
    stats: _MigrateStats,
    *,
    dry_run: bool,
) -> None:
    """Print the plan for one directory and optionally apply it."""
    rel = directory.relative_to(root) if directory != root else Path()
    label = f"notes/{rel}" if str(rel) != "." else "notes/"
    click.echo(f"\n{label}")

    for action in plan.actions:
        if action.skipped:
            click.echo(f"  SKIP    {action.path.name}")
            stats.skipped += 1
        elif action.renamed:
            click.echo(f"  RENAME  {action.path.name} -> {action.target_name}")
            if action.mtime_ordered:
                click.echo(
                    f"  WARN    {action.path.name}  (index inferred from mtime — review ordering)"
                )
                stats.warned += 1
            stats.renamed += 1
        else:
            click.echo(f"  REWRITE {action.path.name}  (frontmatter updated)")
            stats.rewritten += 1

    for error in plan.errors:
        click.echo(f"  ERROR   {error.path.name}: {error.message}", err=True)
        stats.errors += 1

    if not dry_run:
        migrate_mod.apply_plan(plan)


def _run_fzf_search(results: list[SearchResult]) -> int:
    """Run fzf over search results. Returns fzf's exit code."""
    if not results:
        click.echo("No results found.", err=True)
        return 1

    fzf = shutil.which("fzf")
    if fzf is None:
        raise click.ClickException("fzf not found on PATH")

    editor = os.environ.get("EDITOR", "vi")

    lines = []
    for r in results:
        excerpt = r.excerpt or ""
        if r.line_number is not None:
            excerpt = f":{r.line_number}: {excerpt}"
        content = f"{r.note.path.name:<40}  {r.note.meta.name:<35}  {excerpt}"
        line_number = r.line_number or 0
        lines.append(f"{r.note.path}\t{r.score:>3}\t{content}\t{line_number}")

    fzf_input = "\n".join(lines)

    proc = subprocess.run(
        [
            fzf,
            "--delimiter=\t",
            "--with-nth=2,3",
            "--nth=2",
            "--preview=bat -l md --style=plain --color=always {1}",
            "--preview-window=right:60%:wrap",
            f"--bind=enter:become({editor} +{{4}} {{1}})",
            "--expect=ctrl-o",
            "--ansi",
        ],
        input=fzf_input,
        text=True,
        check=False,
        capture_output=False,
        stdout=subprocess.PIPE,
    )

    if proc.returncode != 0:
        return proc.returncode

    output_lines = proc.stdout.splitlines()
    if not output_lines or len(output_lines) == 1:
        return 1

    key, selected = output_lines[0], output_lines[1]
    path = selected.split("\t")[0]

    if key == "ctrl-o":
        pyperclip.copy(path)

    return 0


def _all_project_roots(synctank_dir: Path) -> list[Path]:
    """Return all project directories inside synctank_dir."""
    if not synctank_dir.exists():
        return []
    return [
        d
        for d in sorted(synctank_dir.iterdir())
        if d.is_dir() and not d.name.startswith(".")
    ]


def _ensure_git_excluded(cwd: Path, name: str) -> None:
    """Add symlink name to .git/info/exclude if inside a git repo."""
    git = shutil.which("git")
    if git is None:
        return
    try:
        result = subprocess.run(
            [git, "rev-parse", "--show-toplevel", "--git-path", "info/exclude"],
            cwd=cwd,
            capture_output=True,
            text=True,
            check=True,
        )
    except subprocess.CalledProcessError:
        return

    repo_root, exclude_file = map(Path, result.stdout.strip().split("\n"))
    if not exclude_file.is_absolute():
        exclude_file = (cwd / exclude_file).resolve()

    exclude_file.parent.mkdir(parents=True, exist_ok=True)
    existing = (
        set(exclude_file.read_text().splitlines()) if exclude_file.exists() else set()
    )

    exclude_path = str(cwd.relative_to(repo_root) / name)
    if exclude_path not in existing:
        with exclude_file.open("a") as f:
            f.write(f"{exclude_path}\n")
        click.echo(f"Added {exclude_path} to .git/info/exclude")


if __name__ == "__main__":
    cli()

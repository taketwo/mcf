from __future__ import annotations

from datetime import date
from io import StringIO
from pathlib import Path

from rich.console import Console

from synctank.lint import LintViolation
from synctank.notes import Frontmatter, Note, ParseError, write_note
from synctank.rendering import (
    render_lint_violations,
    render_notes_table,
    render_search_results,
    render_status,
)
from synctank.schema import Kind, Status
from synctank.search import SearchResult

TODAY = date(2026, 4, 22)


def render_to_str(renderable: object) -> str:
    """Render a Rich renderable to a string."""
    console = Console(file=StringIO(), width=120)
    console.print(renderable)
    return console.file.getvalue()  # type: ignore[union-attr]


def make_note(tmp_path: Path, name: str = "Test", kind: Kind = Kind.SPEC) -> Note:
    """Write a note for rendering tests."""
    return write_note(
        tmp_path, Frontmatter(name=name, kind=kind, status=Status.DRAFT, date=TODAY)
    )


class TestRenderNotesTable:
    def test_renders_note_name(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, "Decoder Refactor", Kind.DESIGN)
        output = render_to_str(render_notes_table([note], [], notes_root=tmp_path))
        assert "Decoder Refactor" in output

    def test_renders_directory_header(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, "Test", Kind.SPEC)
        output = render_to_str(render_notes_table([note], [], notes_root=tmp_path))
        assert "notes/" in output

    def test_renders_subdir_header(self, tmp_path: Path) -> None:
        sub = tmp_path / "sub"
        note = write_note(sub, Frontmatter("Sub Note", Kind.PLAN, Status.DRAFT, TODAY))
        output = render_to_str(render_notes_table([note], [], notes_root=tmp_path))
        assert "sub" in output

    def test_renders_parse_errors(self, tmp_path: Path) -> None:
        error = ParseError(path=tmp_path / "bad.md", message="bad frontmatter")
        output = render_to_str(render_notes_table([], [error], notes_root=tmp_path))
        assert "error" in output

    def test_empty_shows_no_notes_message(self, tmp_path: Path) -> None:
        output = render_to_str(render_notes_table([], [], notes_root=tmp_path))
        assert "No notes found" in output

    def test_note_outside_notes_root_uses_absolute_path(self, tmp_path: Path) -> None:
        notes_root = tmp_path / "notes"
        notes_root.mkdir()
        elsewhere = tmp_path / "elsewhere"
        elsewhere.mkdir()
        note = make_note(elsewhere, "Stray Note", Kind.SPEC)
        output = render_to_str(render_notes_table([note], [], notes_root=notes_root))
        assert "Stray Note" in output


class TestRenderSearchResults:
    def test_renders_result_name(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, "Decoder Refactor", Kind.DESIGN)
        result = SearchResult(
            note=note, score=85, excerpt="some excerpt", line_number=5
        )
        output = render_to_str(render_search_results([result]))
        assert "Decoder Refactor" in output

    def test_renders_score(self, tmp_path: Path) -> None:
        note = make_note(tmp_path)
        result = SearchResult(note=note, score=92, excerpt=None, line_number=None)
        output = render_to_str(render_search_results([result]))
        assert "92" in output

    def test_renders_excerpt_with_line_number(self, tmp_path: Path) -> None:
        note = make_note(tmp_path)
        result = SearchResult(
            note=note, score=80, excerpt="matching line", line_number=7
        )
        output = render_to_str(render_search_results([result]))
        assert ":7:" in output
        assert "matching line" in output

    def test_empty_shows_no_results_message(self, tmp_path: Path) -> None:
        output = render_to_str(render_search_results([]))
        assert "No results found" in output


class TestRenderLintViolations:
    def test_renders_violation_message(self, tmp_path: Path) -> None:
        v = LintViolation(path=tmp_path / "001-test-spec.md", message="bad slug")
        output = render_to_str(render_lint_violations([v]))
        assert "bad slug" in output

    def test_no_violations_shows_ok_message(self, tmp_path: Path) -> None:
        output = render_to_str(render_lint_violations([]))
        assert "No violations" in output


class TestRenderStatus:
    def test_not_linked(self, tmp_path: Path) -> None:
        output = render_to_str(render_status(None, tmp_path))
        assert "Not linked" in output

    def test_linked_shows_project(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        project_dir = synctank_dir / "myproject"
        project_dir.mkdir(parents=True)

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "notes").symlink_to(project_dir)

        output = render_to_str(render_status(workspace, synctank_dir))
        assert "myproject" in output

    def test_symlink_outside_synctank_shows_not_linked(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()
        other_dir = tmp_path / "other"
        other_dir.mkdir()

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "notes").symlink_to(other_dir)

        output = render_to_str(render_status(workspace, synctank_dir))
        assert "Not linked" in output

    def test_non_symlink_items_ignored(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "regular_file.txt").write_text("hello")

        output = render_to_str(render_status(workspace, synctank_dir))
        assert "Not linked" in output

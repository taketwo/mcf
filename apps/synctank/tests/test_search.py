from __future__ import annotations

from datetime import date
from pathlib import Path

from synctank.notes import Frontmatter, write_note
from synctank.schema import Kind, Status
from synctank.search import search_notes

TODAY = date(2026, 4, 22)


def make_note(root: Path, name: str, kind: Kind, body: str = "") -> None:
    """Write a note file."""
    write_note(
        root, Frontmatter(name=name, kind=kind, status=Status.DRAFT, date=TODAY), body
    )


class TestSearchNotes:
    def test_returns_results_for_matching_name(self, tmp_path: Path) -> None:
        make_note(tmp_path, "Decoder Refactor", Kind.DESIGN)
        make_note(tmp_path, "Pipeline Setup", Kind.PLAN)

        results = search_notes("decoder", [tmp_path])
        assert len(results) == 2
        assert results[0].note.meta.name == "Decoder Refactor"

    def test_results_sorted_by_score_descending(self, tmp_path: Path) -> None:
        make_note(tmp_path, "Decoder Refactor", Kind.DESIGN)
        make_note(tmp_path, "Pipeline Setup", Kind.PLAN)

        results = search_notes("decoder", [tmp_path])
        assert results[0].score >= results[1].score

    def test_names_only_mode(self, tmp_path: Path) -> None:
        make_note(
            tmp_path, "Decoder Refactor", Kind.DESIGN, body="unrelated content here"
        )

        results = search_notes("decoder", [tmp_path], names_only=True)
        assert len(results) == 1
        assert results[0].excerpt is None
        assert results[0].line_number is None

    def test_content_search_returns_excerpt(self, tmp_path: Path) -> None:
        make_note(
            tmp_path,
            "Pipeline Setup",
            Kind.PLAN,
            body="## Overview\n\nThis covers the decoder implementation details.",
        )

        results = search_notes("decoder implementation", [tmp_path])
        assert len(results) == 1
        assert results[0].excerpt is not None
        assert "decoder" in results[0].excerpt.lower()

    def test_content_search_returns_line_number(self, tmp_path: Path) -> None:
        make_note(
            tmp_path,
            "Pipeline Setup",
            Kind.PLAN,
            body="## Overview\n\nThis covers the decoder implementation.",
        )

        results = search_notes("decoder", [tmp_path])
        assert results[0].line_number is not None
        assert results[0].line_number > 0

    def test_searches_across_multiple_roots(self, tmp_path: Path) -> None:
        root1 = tmp_path / "project1"
        root1.mkdir()
        root2 = tmp_path / "project2"
        root2.mkdir()

        make_note(root1, "Decoder Design", Kind.DESIGN)
        make_note(root2, "Encoder Plan", Kind.PLAN)

        results = search_notes("decoder", [root1, root2])
        assert len(results) == 2

    def test_empty_roots_returns_empty(self, tmp_path: Path) -> None:
        results = search_notes("anything", [tmp_path])
        assert results == []

    def test_skips_unparseable_notes(self, tmp_path: Path) -> None:
        bad = tmp_path / "001-bad-spec.md"
        bad.write_text("---\nkind: bogus\n---\n\n# Bad\n", encoding="utf-8")
        make_note(tmp_path, "Good Note", Kind.SPEC)

        results = search_notes("good", [tmp_path])
        assert len(results) == 1
        assert results[0].note.meta.name == "Good Note"

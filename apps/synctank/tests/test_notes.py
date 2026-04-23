from __future__ import annotations

from datetime import date
from pathlib import Path

import pytest

from synctank.notes import (
    Frontmatter,
    ParseError,
    build_filename,
    enumerate_notes,
    load_note,
    next_index,
    slugify,
    write_note,
)
from synctank.schema import Kind, Status

TODAY = date(2026, 4, 22)


def make_note_file(
    path: Path,
    *,
    name: str = "Test",
    kind: str = "spec",
    status: str = "draft",
    body: str = "",
) -> None:
    """Write a minimal valid note file to path."""
    lines = [
        "---",
        f"name: {name}",
        f"kind: {kind}",
        f"status: {status}",
        "date: 2026-04-22",
        "---",
        "",
        f"# {name}",
    ]
    if body:
        lines += ["", body]
    path.write_text("\n".join(lines), encoding="utf-8")


class TestSlugify:
    @pytest.mark.parametrize(
        ("name", "expected"),
        [
            ("Decoder Refactor", "decoder-refactor"),
            ("LED Tracking Pipeline Design", "led-tracking-pipeline-design"),
            ("  extra  spaces  ", "extra-spaces"),
            ("With/Special*Chars!", "withspecialchars"),
            ("already-slug", "already-slug"),
            ("Multiple---Dashes", "multiple-dashes"),
        ],
    )
    def test_slugify(self, name: str, expected: str) -> None:
        assert slugify(name) == expected


class TestNextIndex:
    @pytest.mark.parametrize(
        ("filenames", "expected"),
        [
            ([], 1),
            (["001-foo-spec.md"], 2),
            (["001-foo-spec.md", "002-bar-design.md"], 3),
            (["001-foo.md", "005-bar.md"], 6),
            (["not-a-note.md", "README.md"], 1),
            (["001-foo.md", "not-a-note.md"], 2),
        ],
    )
    def test_next_index(self, filenames: list[str], expected: int) -> None:
        assert next_index(filenames) == expected


class TestBuildFilename:
    @pytest.mark.parametrize(
        ("index", "slug", "kind", "expected"),
        [
            (1, "decoder-refactor", Kind.DESIGN, "001-decoder-refactor-design.md"),
            (42, "pipeline", Kind.SPEC, "042-pipeline-spec.md"),
            (999, "notes", Kind.OTHER, "999-notes.md"),
            (1, "project-brief", Kind.BRIEF, "001-project-brief-brief.md"),
            (
                1,
                "lessons-learned",
                Kind.LESSONS_LEARNED,
                "001-lessons-learned-lessons-learned.md",
            ),
        ],
    )
    def test_build_filename(
        self, index: int, slug: str, kind: Kind, expected: str
    ) -> None:
        assert build_filename(index, slug, kind) == expected


class TestLoadNote:
    def test_loads_valid_note(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        make_note_file(
            path,
            name="Test",
            kind="spec",
            status="draft",
            body="## Overview\n\nSome content.",
        )
        note = load_note(path)

        assert note.meta.name == "Test"
        assert note.meta.kind == Kind.SPEC
        assert note.meta.status == Status.DRAFT
        assert note.meta.date == date(2026, 4, 22)
        assert note.index == 1
        assert note.slug == "test"
        assert "Some content." in note.body

    def test_loads_related_field(self, tmp_path: Path) -> None:
        path = tmp_path / "002-foo-design.md"
        path.write_text(
            "---\nname: Foo\nkind: design\nstatus: draft\ndate: 2026-04-22\nrelated:\n  - 001-bar-spec.md\n---\n\n# Foo — Design\n",
            encoding="utf-8",
        )
        note = load_note(path)
        assert note.meta.related == ["001-bar-spec.md"]

    def test_preserves_extra_fields(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: draft\ndate: 2026-04-22\nsuperseded-by: 002-new-spec.md\n---\n\n# Test — Spec\n",
            encoding="utf-8",
        )
        note = load_note(path)
        assert note.meta.extra == {"superseded-by": "002-new-spec.md"}

    def test_raises_on_missing_name(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nkind: spec\nstatus: draft\ndate: 2026-04-22\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="name"):
            load_note(path)

    def test_raises_on_invalid_kind(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: bogus\nstatus: draft\ndate: 2026-04-22\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="kind"):
            load_note(path)

    def test_raises_on_invalid_status(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: bogus\ndate: 2026-04-22\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="status"):
            load_note(path)

    def test_raises_on_invalid_date(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: draft\ndate: not-a-date\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="date"):
            load_note(path)

    def test_raises_on_bad_filename(self, tmp_path: Path) -> None:
        path = tmp_path / "no-index-spec.md"
        make_note_file(path)
        with pytest.raises(ParseError, match="filename"):
            load_note(path)

    def test_raises_on_missing_date(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: draft\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="date"):
            load_note(path)

    def test_raises_on_related_non_list(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: draft\ndate: 2026-04-22\nrelated: not-a-list\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="related"):
            load_note(path)

    def test_raises_on_related_non_string_items(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: draft\ndate: 2026-04-22\nrelated:\n  - 42\n---\n\n# Test\n",
            encoding="utf-8",
        )
        with pytest.raises(ParseError, match="related"):
            load_note(path)

    def test_raises_on_corrupt_file(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_bytes(b"\xff\xfe invalid utf content \x00")
        with pytest.raises(ParseError):
            load_note(path)

    def test_empty_body_is_allowed(self, tmp_path: Path) -> None:
        path = tmp_path / "001-test-spec.md"
        path.write_text(
            "---\nname: Test\nkind: spec\nstatus: draft\ndate: 2026-04-22\n---\n\n# Test — Spec\n",
            encoding="utf-8",
        )
        note = load_note(path)
        assert note.body == "# Test — Spec"


class TestEnumerateNotes:
    def test_yields_matching_files(self, tmp_path: Path) -> None:
        make_note_file(tmp_path / "001-foo-spec.md")
        make_note_file(tmp_path / "002-bar-design.md")
        (tmp_path / "README.md").write_text("not a note", encoding="utf-8")

        paths = list(enumerate_notes(tmp_path))
        assert len(paths) == 2
        assert all(p.name.startswith(("001", "002")) for p in paths)

    def test_recursive_by_default(self, tmp_path: Path) -> None:
        sub = tmp_path / "sub"
        sub.mkdir()
        make_note_file(tmp_path / "001-root-spec.md")
        make_note_file(sub / "001-sub-spec.md")

        paths = list(enumerate_notes(tmp_path))
        assert len(paths) == 2

    def test_non_recursive(self, tmp_path: Path) -> None:
        sub = tmp_path / "sub"
        sub.mkdir()
        make_note_file(tmp_path / "001-root-spec.md")
        make_note_file(sub / "001-sub-spec.md")

        paths = list(enumerate_notes(tmp_path, recursive=False))
        assert len(paths) == 1
        assert paths[0].name == "001-root-spec.md"

    def test_skips_non_matching_md_files(self, tmp_path: Path) -> None:
        (tmp_path / "README.md").write_text("", encoding="utf-8")
        (tmp_path / "foo.md").write_text("", encoding="utf-8")
        make_note_file(tmp_path / "001-note-spec.md")

        paths = list(enumerate_notes(tmp_path))
        assert len(paths) == 1


def make_params(
    name: str = "Test",
    kind: Kind = Kind.SPEC,
    status: Status = Status.DRAFT,
    related: list[str] | None = None,
) -> Frontmatter:
    """Build a Frontmatter for testing."""
    return Frontmatter(
        name=name,
        kind=kind,
        status=status,
        date=TODAY,
        related=related or [],
    )


class TestWriteNote:
    def test_creates_file_with_correct_name(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("Decoder Refactor", Kind.DESIGN))
        assert note.path.name == "001-decoder-refactor-design.md"

    def test_increments_index(self, tmp_path: Path) -> None:
        make_note_file(tmp_path / "001-existing-spec.md")
        note = write_note(tmp_path, make_params("New Note", Kind.SPEC))
        assert note.path.name == "002-new-note-spec.md"

    def test_creates_subdir(self, tmp_path: Path) -> None:
        note = write_note(tmp_path / "subsection", make_params("My Note", Kind.BRIEF))
        assert note.path.parent == tmp_path / "subsection"

    def test_file_contains_frontmatter(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("Test Note", Kind.PLAN, Status.LIVING))
        content = note.path.read_text(encoding="utf-8")
        assert "name: Test Note" in content
        assert "kind: plan" in content
        assert "status: living" in content
        assert "date: 2026-04-22" in content

    def test_file_contains_h1(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("Test Note", Kind.PLAN))
        content = note.path.read_text(encoding="utf-8")
        assert "# Test Note — Plan" in content

    def test_file_contains_body(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params(), "## Overview\n\nContent here.")
        content = note.path.read_text(encoding="utf-8")
        assert "## Overview" in content
        assert "Content here." in content

    def test_other_kind_has_no_suffix(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("Misc", Kind.OTHER))
        assert note.path.name == "001-misc.md"

    def test_other_kind_h1_has_no_kind(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("Misc", Kind.OTHER))
        content = note.path.read_text(encoding="utf-8")
        assert "# Misc\n" in content
        assert "—" not in content

    def test_related_in_frontmatter(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params(related=["001-foo-brief.md"]))
        content = note.path.read_text(encoding="utf-8")
        assert "related:" in content
        assert "- 001-foo-brief.md" in content

    def test_written_note_is_loadable(self, tmp_path: Path) -> None:
        note = write_note(
            tmp_path, make_params("Round Trip", Kind.DESIGN), "## Body\n\nContent."
        )
        loaded = load_note(note.path)
        assert loaded.meta.name == "Round Trip"
        assert loaded.meta.kind == Kind.DESIGN
        assert loaded.meta.status == Status.DRAFT

    def test_to_dict(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params())
        d = note.to_dict()
        assert d["name"] == "Test"
        assert d["kind"] == "spec"
        assert d["status"] == "draft"
        assert d["date"] == "2026-04-22"

    @pytest.mark.parametrize(
        "tricky_name",
        [
            "5:30 meeting notes",
            "Notes: a review",
            "- leading dash",
            "? leading question",
            "* leading star",
            "> leading gt",
            "| leading pipe",
            "! leading bang",
            "@ leading at",
            "` leading backtick",
            "[bracketed]",
            '"quoted"',
            "'apostrophed'",
            "line\nbreak in name",
            "tab\there",
            "unicode — em dash",
        ],
    )
    def test_yaml_special_characters_roundtrip(
        self, tmp_path: Path, tricky_name: str
    ) -> None:
        note = write_note(tmp_path, make_params(tricky_name))
        loaded = load_note(note.path)
        assert loaded.meta.name == tricky_name

    def test_extra_fields_roundtrip(self, tmp_path: Path) -> None:
        fm = make_params()
        fm.extra = {"superseded-by": "005-new-spec.md", "priority": 3}
        note = write_note(tmp_path, fm)
        loaded = load_note(note.path)
        assert loaded.meta.extra == {"superseded-by": "005-new-spec.md", "priority": 3}

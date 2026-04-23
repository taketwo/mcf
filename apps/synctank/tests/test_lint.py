from __future__ import annotations

from datetime import date
from pathlib import Path

import pytest

from synctank.lint import (
    LintViolation,
    check_filename_kind,
    check_filename_slug,
    check_heading_case,
    check_no_line_wrapping,
    lint_note,
    lint_path,
)
from synctank.notes import Frontmatter, Note, write_note
from synctank.schema import Kind, Status

TODAY = date(2026, 4, 22)


def make_note(
    tmp_path: Path,
    *,
    name: str = "Test",
    kind: Kind = Kind.SPEC,
    status: Status = Status.DRAFT,
    slug: str | None = None,
    index: int = 1,
    body: str = "",
    filename: str | None = None,
) -> Note:
    """Construct a Note directly without filesystem, for pure rule testing."""
    resolved_slug = slug if slug is not None else name.lower().replace(" ", "-")
    resolved_filename = filename or f"{index:03d}-{resolved_slug}-{kind.value}.md"
    if kind == Kind.OTHER:
        resolved_filename = filename or f"{index:03d}-{resolved_slug}.md"
    return Note(
        path=tmp_path / resolved_filename,
        index=index,
        slug=resolved_slug,
        meta=Frontmatter(name=name, kind=kind, status=status, date=TODAY),
        body=body,
    )


class TestCheckFilenameKind:
    def test_passes_matching_kind(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, kind=Kind.SPEC)
        assert check_filename_kind(note) is None

    def test_passes_other_without_suffix(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, kind=Kind.OTHER)
        assert check_filename_kind(note) is None

    def test_fails_mismatched_kind(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, kind=Kind.DESIGN, filename="001-test-spec.md")
        violation = check_filename_kind(note)
        assert violation is not None
        assert "design" in violation.message

    def test_fails_other_with_kind_suffix(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, kind=Kind.OTHER, filename="001-test-spec.md")
        violation = check_filename_kind(note)
        assert violation is not None
        assert "other" in violation.message

    @pytest.mark.parametrize("kind", [k for k in Kind if k != Kind.OTHER])
    def test_passes_all_non_other_kinds(self, tmp_path: Path, kind: Kind) -> None:
        note = make_note(tmp_path, kind=kind)
        assert check_filename_kind(note) is None

    def test_passes_other_single_segment_stem(self, tmp_path: Path) -> None:
        # Stem with no hyphens (len(parts) == 1): branch skips the kind-suffix check entirely.
        # Unreachable from load_note (the filename regex requires a hyphen), but
        # the branch exists in the rule so we exercise it directly.
        note = make_note(tmp_path, kind=Kind.OTHER, filename="001.md")
        assert check_filename_kind(note) is None


class TestCheckFilenameSlug:
    def test_passes_matching_slug(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, name="Decoder Refactor", slug="decoder-refactor")
        assert check_filename_slug(note) is None

    def test_fails_mismatched_slug(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, name="Decoder Refactor", slug="wrong-slug")
        violation = check_filename_slug(note)
        assert violation is not None
        assert "wrong-slug" in violation.message
        assert "decoder-refactor" in violation.message


class TestCheckNoLineWrapping:
    def test_passes_unwrapped_prose(self, tmp_path: Path) -> None:
        body = "This is a long paragraph that goes on and on without any line breaks in it whatsoever."
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None

    def test_passes_code_block(self, tmp_path: Path) -> None:
        body = "```python\nshort\nlines\nin\ncode\nblock\n```"
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None

    def test_passes_short_body(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="One line.")
        assert check_no_line_wrapping(note) is None

    def test_flags_wrapped_paragraph(self, tmp_path: Path) -> None:
        body = (
            "This paragraph has been\n"
            "hard wrapped at a short\n"
            "column width which is\n"
            "considered bad style here.\n"
        )
        note = make_note(tmp_path, body=body)
        violation = check_no_line_wrapping(note)
        assert violation is not None
        assert "hard-wrapped" in violation.message

    def test_flags_wrapped_paragraph_followed_by_blank(self, tmp_path: Path) -> None:
        body = (
            "This paragraph has been\n"
            "hard wrapped at a short\n"
            "column width which is bad.\n"
            "\n"
            "Second paragraph is fine."
        )
        note = make_note(tmp_path, body=body)
        violation = check_no_line_wrapping(note)
        assert violation is not None

    def test_passes_wrapped_content_inside_code_block(self, tmp_path: Path) -> None:
        body = "```\nshort\nlines\ninside\ncode\nblock\nare\nfine\n```"
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None

    def test_flushes_paragraph_before_code_block(self, tmp_path: Path) -> None:
        body = (
            "This paragraph has been\n"
            "hard wrapped at a short\n"
            "column width which is bad.\n"
            "```\nsome code\n```"
        )
        note = make_note(tmp_path, body=body)
        violation = check_no_line_wrapping(note)
        assert violation is not None

    def test_passes_list_items(self, tmp_path: Path) -> None:
        body = (
            "- First list item that has a medium length for testing\n"
            "- Second list item that has a medium length for testing\n"
            "- Third list item that has a medium length for testing\n"
        )
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None

    def test_passes_prose_before_code_block(self, tmp_path: Path) -> None:
        body = "Short line.\n```\ncode\n```"
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None

    def test_passes_leading_blank_line(self, tmp_path: Path) -> None:
        body = "\n\nSome content here."
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None

    def test_passes_non_wrapped_paragraph_with_blank(self, tmp_path: Path) -> None:
        body = "A single prose line.\n\nAnother paragraph."
        note = make_note(tmp_path, body=body)
        assert check_no_line_wrapping(note) is None


class TestCheckHeadingCase:
    def test_passes_sentence_case(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="## Implementation details\n\nBody.")
        assert check_heading_case(note) is None

    def test_fails_title_case(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="## Implementation Details\n\nBody.")
        violation = check_heading_case(note)
        assert violation is not None
        assert "Details" in violation.message

    def test_passes_code_block_heading(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="```\n## Title Case Inside Code\n```")
        assert check_heading_case(note) is None

    def test_passes_single_word_heading(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="## Overview\n\nBody.")
        assert check_heading_case(note) is None

    def test_passes_all_caps_acronym(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="## Results for LED tracking\n\nBody.")
        assert check_heading_case(note) is None

    def test_passes_empty_heading(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="##\n\nBody.")
        assert check_heading_case(note) is None

    def test_passes_numbered_heading(self, tmp_path: Path) -> None:
        note = make_note(tmp_path, body="## 1. First step\n\n## 2. Second step")
        assert check_heading_case(note) is None


class TestLintNote:
    def test_valid_note_has_no_violations(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, Frontmatter("Test", Kind.SPEC, Status.DRAFT, TODAY))
        violations = lint_note(note.path)
        assert violations == []

    def test_parse_error_reported_as_violation(self, tmp_path: Path) -> None:
        path = tmp_path / "001-bad-spec.md"
        path.write_text("---\nkind: spec\n---\n\n# Bad\n", encoding="utf-8")
        violations = lint_note(path)
        assert len(violations) == 1
        assert "parse error" in violations[0].message


class TestLintPath:
    def test_lints_single_file(self, tmp_path: Path) -> None:
        note = write_note(tmp_path, Frontmatter("Test", Kind.SPEC, Status.DRAFT, TODAY))
        violations = lint_path(note.path)
        assert violations == []

    def test_lints_directory_recursively(self, tmp_path: Path) -> None:
        write_note(tmp_path, Frontmatter("Test One", Kind.SPEC, Status.DRAFT, TODAY))
        sub = tmp_path / "sub"
        sub.mkdir()
        write_note(sub, Frontmatter("Test Two", Kind.DESIGN, Status.DRAFT, TODAY))
        violations = lint_path(tmp_path)
        assert violations == []

    def test_str_representation(self, tmp_path: Path) -> None:
        v = LintViolation(path=tmp_path / "001-test-spec.md", message="something wrong")
        assert "something wrong" in str(v)
        assert "001-test-spec.md" in str(v)

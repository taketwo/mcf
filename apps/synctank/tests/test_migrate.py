from __future__ import annotations

from datetime import UTC, date, datetime
from pathlib import Path
from textwrap import dedent

import pytest

from synctank.migrate import (
    MigrateAction,
    MigratePlan,
    _merge_related,
    _mtime_date,
    _parse_file,
    _sort_key,
    _strip_leading_h1,
    apply_plan,
    infer_kind,
    infer_name,
    plan_directory,
    plan_migration,
)
from synctank.schema import Kind

# ---------------------------------------------------------------------------
# infer_name
# ---------------------------------------------------------------------------


class TestInferName:
    def test_uses_name_field(self, tmp_path: Path) -> None:
        assert infer_name({"name": "My Note"}, tmp_path / "foo.md") == "My Note"

    def test_strips_whitespace_from_name(self, tmp_path: Path) -> None:
        assert infer_name({"name": "  My Note  "}, tmp_path / "foo.md") == "My Note"

    def test_falls_back_to_title(self, tmp_path: Path) -> None:
        assert infer_name({"title": "My Title"}, tmp_path / "foo.md") == "My Title"

    def test_name_takes_priority_over_title(self, tmp_path: Path) -> None:
        assert (
            infer_name({"name": "Name", "title": "Title"}, tmp_path / "foo.md")
            == "Name"
        )

    def test_falls_back_to_filename(self, tmp_path: Path) -> None:
        assert infer_name({}, tmp_path / "my-note.md") == "My Note"

    def test_strips_numeric_prefix_from_filename(self, tmp_path: Path) -> None:
        assert infer_name({}, tmp_path / "01-my-note.md") == "My Note"

    def test_underscores_become_spaces(self, tmp_path: Path) -> None:
        assert infer_name({}, tmp_path / "my_note.md") == "My Note"

    def test_ignores_empty_name_field(self, tmp_path: Path) -> None:
        assert infer_name({"name": "  "}, tmp_path / "fallback.md") == "Fallback"


# ---------------------------------------------------------------------------
# infer_kind
# ---------------------------------------------------------------------------


@pytest.mark.parametrize(
    ("name", "expected"),
    [
        ("Decoder Design", Kind.DESIGN),
        ("System Architecture", Kind.DESIGN),
        ("LED Tracking Pipeline Rethink Project Brief", Kind.BRIEF),
        ("System Context", Kind.BRIEF),
        ("System Overview", Kind.BRIEF),
        ("Motion Bounds Tool Spec", Kind.SPEC),
        ("API Specification", Kind.SPEC),
        ("Implementation Plan", Kind.PLAN),
        ("Release Planning", Kind.PLAN),
        ("Requirements Document", Kind.REQUIREMENTS),
        ("Detector Characterization", Kind.REPORT),
        ("Analysis Report", Kind.REPORT),
        ("Implementation Insights", Kind.REPORT),
        ("Progress Update", Kind.REPORT),
        ("How To Deploy", Kind.GUIDE),
        ("Glossary", Kind.REFERENCE),
        ("Checklist", Kind.REFERENCE),
        ("Brainstorm Session", Kind.BRAINSTORM),
        ("Lessons Learned From Refactor", Kind.LESSONS_LEARNED),
        ("Something Completely Random", Kind.OTHER),
    ],
)
def test_infer_kind(name: str, expected: Kind) -> None:
    assert infer_kind(name) == expected


# ---------------------------------------------------------------------------
# _merge_related
# ---------------------------------------------------------------------------


class TestMergeRelated:
    def test_empty_meta(self) -> None:
        assert _merge_related({}) == []

    def test_related_list(self) -> None:
        assert _merge_related({"related": ["a.md", "b.md"]}) == ["a.md", "b.md"]

    def test_parent_string(self) -> None:
        assert _merge_related({"parent": "a.md"}) == ["a.md"]

    def test_based_on_string(self) -> None:
        assert _merge_related({"based-on": "a.md"}) == ["a.md"]

    def test_merges_all_three(self) -> None:
        result = _merge_related(
            {"parent": "a.md", "based-on": "b.md", "related": ["c.md"]}
        )
        assert result == ["a.md", "b.md", "c.md"]

    def test_deduplicates(self) -> None:
        result = _merge_related({"parent": "a.md", "related": ["a.md", "b.md"]})
        assert result == ["a.md", "b.md"]

    def test_ignores_empty_strings(self) -> None:
        assert _merge_related({"parent": "", "related": ["a.md"]}) == ["a.md"]

    def test_ignores_none_values(self) -> None:
        assert _merge_related({"parent": None}) == []


# ---------------------------------------------------------------------------
# _strip_leading_h1
# ---------------------------------------------------------------------------


class TestStripLeadingH1:
    def test_strips_h1(self) -> None:
        assert _strip_leading_h1("# Title\n\nBody text.") == "Body text."

    def test_no_h1_unchanged(self) -> None:
        assert _strip_leading_h1("## Section\n\nBody.") == "## Section\n\nBody."

    def test_empty_body(self) -> None:
        assert _strip_leading_h1("") == ""

    def test_h1_only(self) -> None:
        assert _strip_leading_h1("# Title") == ""

    def test_strips_leading_blank_lines_before_h1(self) -> None:
        assert _strip_leading_h1("\n\n# Title\n\nBody.") == "Body."

    def test_stops_at_first_non_blank_non_h1(self) -> None:
        body = "Some text\n# Not a leading H1"
        assert _strip_leading_h1(body) == body


# ---------------------------------------------------------------------------
# _parse_file
# ---------------------------------------------------------------------------


class TestParseFile:
    def test_parses_frontmatter_and_body(self) -> None:
        text = dedent("""\
            ---
            title: My Note
            date: 2026-01-01
            ---

            # My Note

            Body text.
        """)
        meta, body = _parse_file(text)
        assert meta["title"] == "My Note"
        assert body == "Body text."

    def test_strips_leading_h1_from_body(self) -> None:
        _, body = _parse_file("---\ntitle: T\n---\n\n# T\n\nBody.")
        assert body == "Body."

    def test_no_frontmatter_returns_empty_meta(self) -> None:
        meta, body = _parse_file("# Title\n\nBody text.")
        assert meta == {}
        assert body == "Body text."

    def test_invalid_yaml_raises_value_error(self) -> None:
        with pytest.raises(ValueError, match="YAML parse error"):
            _parse_file("---\nkey: [unclosed\n---\n\nBody.")


# ---------------------------------------------------------------------------
# _sort_key
# ---------------------------------------------------------------------------


class TestSortKey:
    def test_prefixed_sorts_before_unprefixed(self, tmp_path: Path) -> None:
        a = tmp_path / "01-foo.md"
        b = tmp_path / "bar.md"
        a.touch()
        b.touch()
        assert _sort_key(a) < _sort_key(b)

    def test_prefixed_sorted_numerically(self, tmp_path: Path) -> None:
        a = tmp_path / "02-foo.md"
        b = tmp_path / "10-bar.md"
        a.touch()
        b.touch()
        assert _sort_key(a) < _sort_key(b)


# ---------------------------------------------------------------------------
# _mtime_date
# ---------------------------------------------------------------------------


def test_mtime_date(tmp_path: Path) -> None:
    f = tmp_path / "note.md"
    f.touch()
    result = _mtime_date(f)
    expected = datetime.fromtimestamp(f.stat().st_mtime, tz=UTC).date()
    assert result == expected


# ---------------------------------------------------------------------------
# MigrateAction
# ---------------------------------------------------------------------------


class TestMigrateAction:
    def test_skipped_when_name_and_content_unchanged(self, tmp_path: Path) -> None:
        f = tmp_path / "001-foo-spec.md"
        action = MigrateAction(
            path=f,
            target_name="001-foo-spec.md",
            new_content="content",
            rewritten=False,
        )
        assert action.skipped

    def test_renamed_when_target_name_differs(self, tmp_path: Path) -> None:
        f = tmp_path / "foo.md"
        action = MigrateAction(
            path=f,
            target_name="001-foo-spec.md",
            new_content="content",
            rewritten=False,
        )
        assert action.renamed
        assert not action.rewritten

    def test_rewritten_when_content_differs(self, tmp_path: Path) -> None:
        f = tmp_path / "001-foo-spec.md"
        action = MigrateAction(
            path=f,
            target_name="001-foo-spec.md",
            new_content="new content",
            rewritten=True,
        )
        assert action.rewritten
        assert not action.renamed


# ---------------------------------------------------------------------------
# plan_directory
# ---------------------------------------------------------------------------


class TestPlanDirectory:
    def _write(self, path: Path, content: str) -> Path:
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        return path

    def test_assigns_sequential_indices(self, tmp_path: Path) -> None:
        self._write(tmp_path / "01-alpha.md", "# Alpha\n\nBody.")
        self._write(tmp_path / "02-beta.md", "# Beta\n\nBody.")
        plan = plan_directory(tmp_path)
        names = [a.target_name for a in plan.actions]
        assert names[0].startswith("001-")
        assert names[1].startswith("002-")

    def test_prefixed_files_sorted_before_unprefixed(self, tmp_path: Path) -> None:
        self._write(tmp_path / "01-alpha.md", "# Alpha\n\nBody.")
        self._write(tmp_path / "zz-no-prefix.md", "# Zz\n\nBody.")
        plan = plan_directory(tmp_path)
        assert plan.actions[0].path.name == "01-alpha.md"
        assert plan.actions[1].path.name == "zz-no-prefix.md"

    def test_uses_frontmatter_date_when_present(self, tmp_path: Path) -> None:
        self._write(
            tmp_path / "note.md", "---\ntitle: Note\ndate: 2025-03-15\n---\n\nBody."
        )
        plan = plan_directory(tmp_path)
        assert date(2025, 3, 15).isoformat() in plan.actions[0].new_content

    def test_uses_mtime_when_no_date(self, tmp_path: Path) -> None:
        f = self._write(tmp_path / "note.md", "# Note\n\nBody.")
        expected = datetime.fromtimestamp(f.stat().st_mtime, tz=UTC).date().isoformat()
        plan = plan_directory(tmp_path)
        assert expected in plan.actions[0].new_content

    def test_infers_kind_from_name(self, tmp_path: Path) -> None:
        self._write(
            tmp_path / "01-decoder-spec.md", "---\ntitle: Decoder Spec\n---\n\nBody."
        )
        plan = plan_directory(tmp_path)
        assert "-spec.md" in plan.actions[0].target_name

    def test_merges_parent_into_related(self, tmp_path: Path) -> None:
        self._write(
            tmp_path / "note.md",
            "---\ntitle: Note\nparent: other.md\n---\n\nBody.",
        )
        plan = plan_directory(tmp_path)
        assert "other.md" in plan.actions[0].new_content

    def test_yaml_error_produces_error(self, tmp_path: Path) -> None:
        self._write(tmp_path / "bad.md", "---\nkey: [unclosed\n---\n\nBody.")
        plan = plan_directory(tmp_path)
        assert len(plan.errors) == 1
        assert "YAML" in plan.errors[0].message

    def test_strips_h1_from_body(self, tmp_path: Path) -> None:
        self._write(
            tmp_path / "note.md", "---\ntitle: My Note\n---\n\n# My Note\n\nBody text."
        )
        plan = plan_directory(tmp_path)
        content = plan.actions[0].new_content
        assert content.count("# My Note") == 1

    def test_extra_fields_preserved(self, tmp_path: Path) -> None:
        self._write(
            tmp_path / "note.md", "---\ntitle: Note\ncustom: value\n---\n\nBody."
        )
        plan = plan_directory(tmp_path)
        assert "custom: value" in plan.actions[0].new_content

    def test_skipped_when_already_compliant(self, tmp_path: Path) -> None:
        self._write(tmp_path / "01-my-spec.md", "---\ntitle: My Spec\n---\n\nBody.")
        plan = plan_directory(tmp_path)
        apply_plan(plan)
        plan2 = plan_directory(tmp_path)
        assert all(a.skipped for a in plan2.actions)


# ---------------------------------------------------------------------------
# plan_migration
# ---------------------------------------------------------------------------


class TestPlanMigration:
    def test_includes_root(self, tmp_path: Path) -> None:
        (tmp_path / "note.md").write_text("# Note\n\nBody.")
        plans = plan_migration(tmp_path)
        assert tmp_path in plans

    def test_includes_subdirectory(self, tmp_path: Path) -> None:
        sub = tmp_path / "sub"
        sub.mkdir()
        (sub / "note.md").write_text("# Note\n\nBody.")
        plans = plan_migration(tmp_path)
        assert sub in plans

    def test_skips_hidden_directories(self, tmp_path: Path) -> None:
        hidden = tmp_path / ".hidden"
        hidden.mkdir()
        (hidden / "note.md").write_text("# Note\n\nBody.")
        plans = plan_migration(tmp_path)
        assert hidden not in plans

    def test_skips_directories_with_no_md_files(self, tmp_path: Path) -> None:
        empty = tmp_path / "empty"
        empty.mkdir()
        plans = plan_migration(tmp_path)
        assert empty not in plans


# ---------------------------------------------------------------------------
# apply_plan
# ---------------------------------------------------------------------------


class TestApplyPlan:
    def test_renames_file(self, tmp_path: Path) -> None:
        f = tmp_path / "old.md"
        f.write_text("old content")
        action = MigrateAction(
            path=f,
            target_name="001-new-spec.md",
            new_content="new content",
            rewritten=True,
        )
        apply_plan(MigratePlan(actions=[action]))
        assert (tmp_path / "001-new-spec.md").exists()
        assert not f.exists()

    def test_rewrites_content(self, tmp_path: Path) -> None:
        f = tmp_path / "001-note-spec.md"
        f.write_text("old content")
        action = MigrateAction(
            path=f,
            target_name="001-note-spec.md",
            new_content="new content",
            rewritten=True,
        )
        apply_plan(MigratePlan(actions=[action]))
        assert f.read_text() == "new content"

    def test_skips_compliant_file(self, tmp_path: Path) -> None:
        f = tmp_path / "001-note-spec.md"
        f.write_text("content")
        action = MigrateAction(
            path=f,
            target_name="001-note-spec.md",
            new_content="content",
            rewritten=False,
        )
        apply_plan(MigratePlan(actions=[action]))
        assert f.read_text() == "content"

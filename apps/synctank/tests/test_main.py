import json
from datetime import date
from pathlib import Path

import pytest
from click.testing import CliRunner

from synctank.main import cli
from synctank.notes import Frontmatter, load_note, write_note
from synctank.schema import Kind, Status

TODAY = date(2026, 4, 22)


def make_params(
    name: str = "Test",
    kind: Kind = Kind.SPEC,
    status: Status = Status.DRAFT,
) -> Frontmatter:
    return Frontmatter(name=name, kind=kind, status=status, date=TODAY)


def test_help(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["--help"])
    assert result.exit_code == 0


def test_debug_flag(runner: CliRunner) -> None:
    result = runner.invoke(cli, ["--debug", "--help"])
    assert result.exit_code == 0


class TestUpdateCommand:
    def test_updates_status(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("My Note", Kind.SPEC, Status.DRAFT))
        result = runner.invoke(cli, ["update", str(note.path), "--status", "complete"])
        assert result.exit_code == 0
        assert load_note(note.path).meta.status == Status.COMPLETE

    def test_prints_path_on_success(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params())
        result = runner.invoke(cli, ["update", str(note.path), "--status", "complete"])
        assert result.exit_code == 0
        assert str(note.path) in result.output

    def test_prints_new_path_after_rename(
        self, runner: CliRunner, tmp_path: Path
    ) -> None:
        note = write_note(tmp_path, make_params("Old Name", Kind.SPEC))
        result = runner.invoke(cli, ["update", str(note.path), "--name", "New Name"])
        assert result.exit_code == 0
        assert "001-new-name-spec.md" in result.output

    def test_json_output(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params())
        result = runner.invoke(
            cli, ["update", str(note.path), "--status", "complete", "--json"]
        )
        assert result.exit_code == 0
        data = json.loads(result.output)
        assert data["status"] == "complete"

    def test_updates_kind(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("My Note", Kind.SPEC))
        result = runner.invoke(cli, ["update", str(note.path), "--kind", "design"])
        assert result.exit_code == 0
        new_path = note.path.parent / "001-my-note-design.md"
        assert load_note(new_path).meta.kind == Kind.DESIGN

    def test_updates_name(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params("Old Name", Kind.SPEC))
        result = runner.invoke(cli, ["update", str(note.path), "--name", "New Name"])
        assert result.exit_code == 0
        new_path = note.path.parent / "001-new-name-spec.md"
        assert load_note(new_path).meta.name == "New Name"

    def test_updates_related(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params())
        result = runner.invoke(
            cli, ["update", str(note.path), "--related", "002-other-spec.md"]
        )
        assert result.exit_code == 0
        assert load_note(note.path).meta.related == ["002-other-spec.md"]

    def test_related_not_provided_preserves_existing(
        self, runner: CliRunner, tmp_path: Path
    ) -> None:
        fm = make_params()
        fm.related = ["002-other-spec.md"]
        note = write_note(tmp_path, fm)
        result = runner.invoke(cli, ["update", str(note.path), "--status", "complete"])
        assert result.exit_code == 0
        assert load_note(note.path).meta.related == ["002-other-spec.md"]

    def test_fails_with_no_options(self, runner: CliRunner, tmp_path: Path) -> None:
        note = write_note(tmp_path, make_params())
        result = runner.invoke(cli, ["update", str(note.path)])
        assert result.exit_code != 0
        assert "Nothing to update" in result.output

    def test_fails_on_nonexistent_file(self, runner: CliRunner, tmp_path: Path) -> None:
        result = runner.invoke(
            cli, ["update", str(tmp_path / "nonexistent.md"), "--status", "complete"]
        )
        assert result.exit_code != 0

    @pytest.mark.parametrize("invalid_status", ["done", "wip", "archived"])
    def test_rejects_invalid_status(
        self, runner: CliRunner, tmp_path: Path, invalid_status: str
    ) -> None:
        note = write_note(tmp_path, make_params())
        result = runner.invoke(
            cli, ["update", str(note.path), "--status", invalid_status]
        )
        assert result.exit_code != 0

    @pytest.mark.parametrize("invalid_kind", ["todo", "note", "doc"])
    def test_rejects_invalid_kind(
        self, runner: CliRunner, tmp_path: Path, invalid_kind: str
    ) -> None:
        note = write_note(tmp_path, make_params())
        result = runner.invoke(cli, ["update", str(note.path), "--kind", invalid_kind])
        assert result.exit_code != 0

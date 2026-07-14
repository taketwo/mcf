import json
from datetime import date
from pathlib import Path

import pytest
from click.testing import CliRunner

from synctank.main import cli
from synctank.notes import Frontmatter, load_note, write_note
from synctank.schema import Kind, Status

TODAY = date(2026, 4, 22)


def _make_store(tmp_path: Path) -> tuple[Path, dict]:
    """Create a store at tmp_path/store/myproject, return (notes_root, env)."""
    notes_root = tmp_path / "store" / "myproject"
    notes_root.mkdir(parents=True)
    env = {"SYNCTANK_DIR": str(tmp_path / "store")}
    return notes_root, env


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


class TestCreateCommand:
    def test_strips_trailing_kind_from_name(
        self, runner: CliRunner, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        notes_root, env = _make_store(tmp_path)
        monkeypatch.chdir(notes_root)
        result = runner.invoke(
            cli,
            [
                "create",
                "Decoder refactor design",
                "--kind",
                "design",
                "--status",
                "draft",
            ],
            env=env,
        )
        assert result.exit_code == 0
        note = load_note(Path(result.output.strip()))
        assert note.meta.name == "Decoder refactor"
        assert note.path.name == "001-decoder-refactor-design.md"

    def test_aborts_on_kind_mismatch(
        self, runner: CliRunner, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        notes_root, env = _make_store(tmp_path)
        monkeypatch.chdir(notes_root)
        result = runner.invoke(
            cli,
            [
                "create",
                "Decoder refactor design",
                "--kind",
                "spec",
                "--status",
                "draft",
            ],
            env=env,
        )
        assert result.exit_code != 0
        assert "ends with kind 'design'" in result.output
        assert "spec" in result.output


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

    def test_aborts_when_name_kind_mismatch(
        self, runner: CliRunner, tmp_path: Path
    ) -> None:
        note = write_note(tmp_path, make_params("My Note", Kind.SPEC))
        result = runner.invoke(
            cli, ["update", str(note.path), "--name", "Decoder refactor design"]
        )
        assert result.exit_code != 0
        assert "ends with kind 'design'" in result.output


class TestListCommand:
    def _invoke(
        self,
        runner: CliRunner,
        notes_root: Path,
        env: dict,
        monkeypatch: pytest.MonkeyPatch,
        args: list[str] | None = None,
    ) -> list[dict]:
        monkeypatch.chdir(notes_root)
        result = runner.invoke(
            cli, ["list", *(args or []), "--json"], env=env, catch_exceptions=False
        )
        assert result.exit_code == 0
        return json.loads(result.output)

    def test_subdir_empty_for_root_notes(
        self, runner: CliRunner, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        notes_root, env = _make_store(tmp_path)
        write_note(notes_root, make_params("Root note"))
        items = self._invoke(runner, notes_root, env, monkeypatch)
        assert items[0]["subdir"] == ""

    def test_subdir_name_for_subdir_notes(
        self, runner: CliRunner, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        notes_root, env = _make_store(tmp_path)
        write_note(notes_root / "pipeline-rethink", make_params("Sub note"))
        items = self._invoke(runner, notes_root, env, monkeypatch)
        assert items[0]["subdir"] == "pipeline-rethink"

    def test_group_sort_older_group_first(
        self, runner: CliRunner, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        """The group whose max note date is older sorts before the newer group."""
        notes_root, env = _make_store(tmp_path)
        date_old = date(2026, 1, 1)
        date_new = date(2026, 6, 1)

        # Root group max = date_new (has both an old and a new note)
        write_note(
            notes_root, Frontmatter("Root old", Kind.SPEC, Status.DRAFT, date_old)
        )
        write_note(
            notes_root, Frontmatter("Root new", Kind.SPEC, Status.DRAFT, date_new)
        )
        # Archive group max = date_old (only one note)
        write_note(
            notes_root / "archive",
            Frontmatter("Archive", Kind.SPEC, Status.DRAFT, date_old),
        )

        items = self._invoke(runner, notes_root, env, monkeypatch)

        assert [i["subdir"] for i in items] == ["archive", "", ""]
        assert [i["index"] for i in items[1:]] == [1, 2]  # root group: ascending index

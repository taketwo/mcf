import json
import os
import subprocess
from datetime import date
from pathlib import Path

import pytest
from click.testing import CliRunner

from synctank.main import _run_fzf_search, cli
from synctank.notes import Frontmatter, load_note, write_note
from synctank.schema import Kind, Status
from synctank.search import SearchResult

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


class TestFzfSearch:
    """Tests for _run_fzf_search subprocess orchestration.

    The editor must be launched as its own subprocess with the terminal
    attached; launching it from inside fzf (via `become`) makes it inherit
    fzf's piped stdout and hang.
    """

    def _result(self, tmp_path: Path, line_number: int | None) -> SearchResult:
        write_note(
            tmp_path,
            Frontmatter(
                name="Decoder Refactor",
                kind=Kind.DESIGN,
                status=Status.DRAFT,
                date=date(2026, 4, 22),
            ),
            "body text",
        )
        note = load_note(next(tmp_path.glob("*.md")))
        return SearchResult(
            note=note, score=90, excerpt="body text", line_number=line_number
        )

    def _run(self, mocker, results, key: str, line_number: int):
        """Drive _run_fzf_search with a stubbed fzf; return (rc, subprocess calls)."""
        path = results[0].note.path
        selected = f"{path}\t 90\tcontent\t{line_number}"

        def fake_run(argv, **kwargs):
            if "fzf" in str(argv[0]):
                return subprocess.CompletedProcess(
                    argv, 0, stdout=f"{key}\n{selected}\n"
                )
            return subprocess.CompletedProcess(argv, 0)

        run = mocker.patch("synctank.main.subprocess.run", side_effect=fake_run)
        mocker.patch(
            "synctank.main.shutil.which", side_effect=lambda n: f"/usr/bin/{n}"
        )
        mocker.patch.dict(os.environ, {"EDITOR": "nvim"})
        rc = _run_fzf_search(results)
        return rc, run.call_args_list

    def test_enter_launches_editor_as_separate_process(self, tmp_path, mocker) -> None:
        results = [self._result(tmp_path, 7)]
        rc, calls = self._run(mocker, results, key="", line_number=7)

        assert rc == 0
        assert len(calls) == 2, "expected an fzf call and a separate editor call"

        editor_argv = calls[1].args[0]
        assert editor_argv == ["nvim", "+7", str(results[0].note.path)]

    def test_editor_stdout_is_not_piped(self, tmp_path, mocker) -> None:
        """The editor needs the real terminal; a piped stdout is what hung it."""
        results = [self._result(tmp_path, 7)]
        _, calls = self._run(mocker, results, key="", line_number=7)

        assert "stdout" not in calls[1].kwargs
        assert "capture_output" not in calls[1].kwargs

    def test_fzf_does_not_use_become_binding(self, tmp_path, mocker) -> None:
        results = [self._result(tmp_path, 7)]
        _, calls = self._run(mocker, results, key="", line_number=7)

        fzf_argv = calls[0].args[0]
        assert not any("become" in arg for arg in fzf_argv)

    def test_filename_match_opens_without_line_number(self, tmp_path, mocker) -> None:
        results = [self._result(tmp_path, None)]
        _, calls = self._run(mocker, results, key="", line_number=0)

        assert calls[1].args[0] == ["nvim", str(results[0].note.path)]

    def test_ctrl_o_copies_path_without_opening_editor(self, tmp_path, mocker) -> None:
        results = [self._result(tmp_path, 7)]
        copy = mocker.patch("synctank.main.pyperclip.copy")
        rc, calls = self._run(mocker, results, key="ctrl-o", line_number=7)

        assert rc == 0
        copy.assert_called_once_with(str(results[0].note.path))
        assert len(calls) == 1, "editor must not be launched for ctrl-o"

    def test_preview_falls_back_to_cat_without_bat(self, tmp_path, mocker) -> None:
        """fzf must get a usable preview command when bat is not installed."""
        results = [self._result(tmp_path, 7)]
        path = results[0].note.path

        def fake_run(argv, **kwargs):
            return subprocess.CompletedProcess(
                argv, 0, stdout=f"\n{path}\t 90\tcontent\t7\n"
            )

        run = mocker.patch("synctank.main.subprocess.run", side_effect=fake_run)
        mocker.patch(
            "synctank.main.shutil.which",
            side_effect=lambda n: None if n == "bat" else f"/usr/bin/{n}",
        )
        mocker.patch.dict(os.environ, {"EDITOR": "nvim"})
        _run_fzf_search(results)

        fzf_argv = run.call_args_list[0].args[0]
        preview = next(a for a in fzf_argv if a.startswith("--preview="))
        assert "bat" not in preview
        assert preview == "--preview=cat {1}"

    def test_no_results_returns_error(self, mocker) -> None:
        mocker.patch("synctank.main.shutil.which", return_value="/usr/bin/fzf")
        assert _run_fzf_search([]) == 1

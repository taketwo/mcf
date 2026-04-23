from pathlib import Path

import click
import pytest

from synctank.workspace import (
    find_notes_symlink,
    find_workspace_root,
    get_synctank_dir,
    resolve_notes_root,
)


class TestFindWorkspaceRoot:
    def test_finds_symlink_in_cwd(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        project_dir = synctank_dir / "myproject"
        project_dir.mkdir(parents=True)

        cwd = tmp_path / "myproject"
        cwd.mkdir()
        (cwd / "notes").symlink_to(project_dir)

        assert find_workspace_root(cwd, synctank_dir) == cwd

    def test_finds_symlink_in_ancestor(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        project_dir = synctank_dir / "myproject"
        project_dir.mkdir(parents=True)

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "notes").symlink_to(project_dir)

        cwd = workspace / "src" / "deep"
        cwd.mkdir(parents=True)

        assert find_workspace_root(cwd, synctank_dir) == workspace

    def test_returns_closest_ancestor(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        outer_dir = synctank_dir / "outer"
        inner_dir = synctank_dir / "inner"
        outer_dir.mkdir(parents=True)
        inner_dir.mkdir(parents=True)

        outer = tmp_path / "outer"
        outer.mkdir()
        (outer / "notes").symlink_to(outer_dir)

        inner = outer / "subproject"
        inner.mkdir()
        (inner / "notes").symlink_to(inner_dir)

        cwd = inner / "src"
        cwd.mkdir()

        assert find_workspace_root(cwd, synctank_dir) == inner

    def test_returns_none_when_no_symlink(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()

        cwd = tmp_path / "unrelated"
        cwd.mkdir()

        assert find_workspace_root(cwd, synctank_dir) is None

    def test_ignores_symlinks_not_pointing_into_synctank(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()

        other_dir = tmp_path / "other"
        other_dir.mkdir()

        cwd = tmp_path / "myproject"
        cwd.mkdir()
        (cwd / "link").symlink_to(other_dir)

        assert find_workspace_root(cwd, synctank_dir) is None

    @pytest.mark.parametrize("link_name", ["notes", "docs", "journal"])
    def test_works_with_any_symlink_name(self, tmp_path: Path, link_name: str) -> None:
        synctank_dir = tmp_path / "Synctank"
        project_dir = synctank_dir / "myproject"
        project_dir.mkdir(parents=True)

        cwd = tmp_path / "myproject"
        cwd.mkdir()
        (cwd / link_name).symlink_to(project_dir)

        assert find_workspace_root(cwd, synctank_dir) == cwd


class TestGetSynctankDir:
    def test_returns_default(self, monkeypatch: pytest.MonkeyPatch) -> None:
        monkeypatch.delenv("SYNCTANK_DIR", raising=False)
        assert get_synctank_dir().name == "Synctank"

    def test_respects_env_var(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ) -> None:
        monkeypatch.setenv("SYNCTANK_DIR", str(tmp_path))
        assert get_synctank_dir() == tmp_path


class TestFindNotesSymlink:
    def test_returns_target_of_synctank_symlink(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        project_dir = synctank_dir / "myproject"
        project_dir.mkdir(parents=True)

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "notes").symlink_to(project_dir)

        assert find_notes_symlink(workspace, synctank_dir) == project_dir.resolve()

    def test_returns_none_when_no_synctank_symlink(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()

        workspace = tmp_path / "myproject"
        workspace.mkdir()

        assert find_notes_symlink(workspace, synctank_dir) is None

    def test_ignores_symlinks_outside_synctank(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()
        other = tmp_path / "other"
        other.mkdir()

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "link").symlink_to(other)

        assert find_notes_symlink(workspace, synctank_dir) is None


class TestResolveNotesRoot:
    def test_returns_notes_root(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        project_dir = synctank_dir / "myproject"
        project_dir.mkdir(parents=True)

        workspace = tmp_path / "myproject"
        workspace.mkdir()
        (workspace / "notes").symlink_to(project_dir)

        assert resolve_notes_root(workspace, synctank_dir) == project_dir.resolve()

    def test_raises_when_not_in_workspace(self, tmp_path: Path) -> None:
        synctank_dir = tmp_path / "Synctank"
        synctank_dir.mkdir()
        cwd = tmp_path / "unrelated"
        cwd.mkdir()

        with pytest.raises(
            click.ClickException, match="Not inside a synctank workspace"
        ):
            resolve_notes_root(cwd, synctank_dir)

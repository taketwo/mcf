import os
from pathlib import Path

import click


def get_synctank_dir() -> Path:
    """Return the base Synctank directory from $SYNCTANK_DIR or ~/Synctank."""
    return Path(os.environ.get("SYNCTANK_DIR", "~/Synctank")).expanduser()


def find_workspace_root(cwd: Path, synctank_dir: Path) -> Path | None:
    """Walk up from cwd toward the filesystem root.

    At each level, look for a symlink pointing into synctank_dir.
    Return the directory at the closest level where one is found, or None.
    """
    current = cwd.resolve()
    synctank_dir = synctank_dir.resolve()

    while True:
        for item in current.iterdir():
            if item.is_symlink():
                target = item.resolve()
                try:
                    target.relative_to(synctank_dir)
                except ValueError:
                    continue
                else:
                    return current

        parent = current.parent
        if parent == current:
            return None
        current = parent


def find_notes_symlink(workspace_root: Path, synctank_dir: Path) -> Path | None:
    """Return the resolved notes symlink target in workspace_root, or None."""
    resolved = synctank_dir.resolve()
    for item in workspace_root.iterdir():
        if item.is_symlink():
            target = item.resolve()
            try:
                target.relative_to(resolved)
            except ValueError:
                continue
            else:
                return target
    return None


def resolve_notes_root(cwd: Path, synctank_dir: Path) -> Path:
    """Return the resolved notes root for the current workspace.

    When cwd is inside the store (synctank_dir/<project>/...), the first path
    component names the project and resolves its notes root directly. Otherwise
    cwd is treated as a project working directory and we look for a symlink
    pointing into the store. Raises click.ClickException if neither applies or
    the notes symlink cannot be found.
    """
    cwd = cwd.resolve()
    synctank_dir = synctank_dir.resolve()

    try:
        relative = cwd.relative_to(synctank_dir)
    except ValueError:
        relative = None

    if relative is not None:
        # Inside the store: the first component must name a single project.
        project = relative.parts[0] if relative.parts else None
        if project is None or project.startswith("."):
            # Bare store root, or a dot-directory like Syncthing's .stversions.
            raise click.ClickException(
                "Inside the notes store but not within a single project. "
                "cd into a project directory under "
                f"{synctank_dir} (e.g. {synctank_dir}/<project>), or run "
                "synctank from your project working directory."
            )
        return synctank_dir / project

    root = find_workspace_root(cwd, synctank_dir)
    if root is None:
        raise click.ClickException(
            "Not inside a synctank workspace. Run 'synctank setup' first."
        )
    notes_root = find_notes_symlink(root, synctank_dir)
    if (
        notes_root is None
    ):  # pragma: no cover - find_workspace_root already matched a symlink here
        raise click.ClickException("Could not locate notes directory.")
    return notes_root

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

    Raises click.ClickException if not inside a synctank workspace or the
    notes symlink cannot be found.
    """
    root = find_workspace_root(cwd, synctank_dir)
    if root is None:
        raise click.ClickException(
            "Not inside a synctank workspace. Run 'synctank setup' first."
        )
    notes_root = find_notes_symlink(root, synctank_dir)
    if notes_root is None:
        raise click.ClickException("Could not locate notes directory.")
    return notes_root

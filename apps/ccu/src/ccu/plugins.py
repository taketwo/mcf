"""Plugin and marketplace alignment utilities."""

import json
import subprocess
from pathlib import Path


def get_marketplaces() -> set[str]:
    """Get currently installed marketplaces.

    Returns
    -------
    set[str]
        Set of GitHub repos in format "owner/repo"

    """
    marketplaces_file = Path.home() / ".claude" / "plugins" / "known_marketplaces.json"
    if not marketplaces_file.exists():
        return set()

    with marketplaces_file.open() as f:
        data = json.load(f)

    result = set()
    for info in data.values():
        if info.get("source", {}).get("source") == "github":
            result.add(info["source"]["repo"])
    return result


def get_plugins() -> set[str]:
    """Get currently installed plugins.

    Returns
    -------
    set[str]
        Set of plugin names in format "plugin@marketplace"

    """
    plugins_file = Path.home() / ".claude" / "plugins" / "installed_plugins.json"
    if not plugins_file.exists():
        return set()

    with plugins_file.open() as f:
        data = json.load(f)

    return set(data.get("plugins", {}).keys())


def add_marketplace(repo: str) -> tuple[bool, str]:
    """Add a marketplace via claude CLI.

    Parameters
    ----------
    repo : str
        GitHub repo in format "owner/repo"

    Returns
    -------
    tuple[bool, str]
        Tuple of (success, error_message)

    """
    try:
        result = subprocess.run(
            ["claude", "plugin", "marketplace", "add", repo],  # noqa: S607
            capture_output=True,
            text=True,
            check=False,
        )
    except (OSError, subprocess.SubprocessError) as e:
        return False, str(e)
    else:
        if result.returncode == 0:
            return True, ""
        return False, result.stderr or result.stdout


def add_plugin(plugin_spec: str) -> tuple[bool, str]:
    """Install a plugin via claude CLI.

    Parameters
    ----------
    plugin_spec : str
        Plugin specification in format "plugin@marketplace"

    Returns
    -------
    tuple[bool, str]
        Tuple of (success, error_message)

    """
    try:
        result = subprocess.run(
            ["claude", "plugin", "install", plugin_spec],  # noqa: S607
            capture_output=True,
            text=True,
            check=False,
        )
    except (OSError, subprocess.SubprocessError) as e:
        return False, str(e)
    else:
        if result.returncode == 0:
            return True, ""
        return False, result.stderr or result.stdout

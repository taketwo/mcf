"""GitHub API integration for nvim-manager."""

from dataclasses import dataclass
from datetime import datetime, UTC

import requests

from .logging import get_logger

logger = get_logger(__name__)


@dataclass(frozen=True)
class CommitInfo:
    """Commit information from GitHub API."""

    hash: str
    date: datetime


def get_commit_info(repo: str, commit_hash: str | None = None) -> CommitInfo:
    """Get commit information from GitHub API.

    Fetches commit details and extracts hash and commit datetime. If no commit_hash
    is provided, fetches the latest commit from the default branch.

    Parameters
    ----------
    repo : str
        Repository in format "owner/name".
    commit_hash : str | None
        Git commit hash to query (can be partial). If None, fetches latest commit.

    Returns
    -------
    CommitInfo
        Commit hash (full SHA) and date (datetime object in UTC).

    Raises
    ------
    requests.RequestException
        If GitHub API request fails.
    KeyError
        If response doesn't contain expected fields.

    """
    if commit_hash is None:
        url = f"https://api.github.com/repos/{repo}/commits/HEAD"
        logger.debug("Fetching latest commit from GitHub API: %s", url)
    else:

        url = f"https://api.github.com/repos/{repo}/commits/{commit_hash}"
        logger.debug("Fetching commit info from GitHub API: %s", url)

    response = requests.get(url, timeout=30)
    response.raise_for_status()
    data = response.json()

    full_hash = str(data["sha"])
    commit_date_iso = str(data["commit"]["committer"]["date"])

    # Parse ISO date to datetime object, assuming UTC if no timezone info included
    commit_date = datetime.fromisoformat(commit_date_iso)
    if commit_date.tzinfo is None:
        commit_date = commit_date.replace(tzinfo=UTC)
    else:
        commit_date = commit_date.astimezone(UTC)

    logger.debug("Commit info fetched, hash: %s, date: %s", full_hash, commit_date)
    return CommitInfo(hash=full_hash, date=commit_date)

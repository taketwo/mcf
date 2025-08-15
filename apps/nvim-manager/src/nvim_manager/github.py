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
    is provided, fetches the latest commit from the default branch. For commit hashes,
    uses GitHub's search API for reliable resolution of partial hashes.

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
    RuntimeError
        If search returns no matches or multiple matches.

    """
    if commit_hash is None:
        # For latest commit, use direct API
        url = f"https://api.github.com/repos/{repo}/commits/HEAD"
        logger.debug("Fetching latest commit from GitHub API: %s", url)
        response = requests.get(url, timeout=30)
        response.raise_for_status()
        data = response.json()
    else:
        # For specific commits, use search API for reliable partial hash resolution
        search_url = (
            f"https://api.github.com/search/commits?q=repo:{repo}+hash:{commit_hash}"
        )
        logger.debug("Searching for commit via GitHub search API: %s", search_url)
        response = requests.get(
            search_url,
            timeout=30,
            headers={"Accept": "application/vnd.github.cloak-preview"},
        )
        response.raise_for_status()
        search_data = response.json()

        total_count = search_data.get("total_count", 0)
        if total_count == 0:
            msg = f"No commit found for hash: {commit_hash}"
            raise RuntimeError(msg)
        if total_count > 1:
            msg = (
                f"Multiple commits found for hash: {commit_hash} (found {total_count})"
            )
            raise RuntimeError(msg)

        data = search_data["items"][0]

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


def get_file_diff(
    repo: str,
    base_commit: str,
    head_commit: str,
    file_path: str,
) -> str | None:
    """Get diff for a specific file between two commits.

    Uses GitHub compare API to fetch diff content for a specific file between two
    commits. Returns None if no changes found or API call fails.

    Parameters
    ----------
    repo : str
        Repository in format "owner/name".
    base_commit : str
        Base commit hash (older version).
    head_commit : str
        Head commit hash (newer version).
    file_path : str
        Path to file within repository (e.g., "runtime/doc/news.txt").

    Returns
    -------
    str | None
        Diff content for the specific file, or None if no changes or error.

    """
    if base_commit == head_commit:
        logger.debug("Base and head commits are identical, no diff needed")
        return None

    compare_url = (
        f"https://api.github.com/repos/{repo}/compare/{base_commit}...{head_commit}"
    )
    logger.debug("Fetching file diff from GitHub API: %s", compare_url)

    try:
        response = requests.get(
            compare_url,
            timeout=30,
            headers={"Accept": "application/vnd.github.v3.diff"},
        )
        response.raise_for_status()
        full_diff = response.text

        # Extract diff for specific file
        diff_lines = full_diff.split("\n")
        file_diff_lines = []
        in_target_file = False

        for line in diff_lines:
            # Check for file header
            if line.startswith("diff --git") and file_path in line:
                in_target_file = True
                file_diff_lines.append(line)
            elif line.startswith("diff --git") and in_target_file:
                # Started a new file, stop collecting
                break
            elif in_target_file:
                file_diff_lines.append(line)

        if not file_diff_lines:
            logger.debug("No changes found for file %s", file_path)
        else:
            file_diff = "\n".join(file_diff_lines)
            logger.debug(
                "Found diff for file %s (%d lines)",
                file_path,
                len(file_diff_lines),
            )
            return file_diff

    except requests.RequestException as e:
        logger.debug("Failed to fetch diff from GitHub API: %s", e)

    return None

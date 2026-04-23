from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import TYPE_CHECKING

from rapidfuzz import fuzz

from .notes import enumerate_notes, load_note

_log = logging.getLogger(__name__)

if TYPE_CHECKING:
    from pathlib import Path

    from .notes import Note


@dataclass
class SearchResult:
    """A note matching a search query."""

    note: Note
    score: int
    excerpt: str | None
    line_number: int | None


def search_notes(
    query: str,
    roots: list[Path],
    *,
    names_only: bool = False,
) -> list[SearchResult]:
    """Fuzzy search notes under the given roots. Results sorted by score descending.

    Searches filenames by default. With names_only=False (default), also searches
    body content and returns the best-matching line as an excerpt with its line number.
    When run with names_only=True, excerpt and line_number are None.
    """
    results: list[SearchResult] = []

    for root in roots:
        for path in enumerate_notes(root):
            try:
                note = load_note(path)
            except Exception as e:  # noqa: BLE001
                _log.debug("skipping %s: %s", path, e)
                continue

            if names_only:
                score = int(fuzz.partial_ratio(query, path.name))
                results.append(
                    SearchResult(note=note, score=score, excerpt=None, line_number=None)
                )
            else:
                result = _search_content(query, note)
                results.append(result)

    results.sort(key=lambda r: r.score, reverse=True)
    return results


def _search_content(query: str, note: Note) -> SearchResult:
    """Score a note by searching its filename and body content."""
    name_score = int(fuzz.partial_ratio(query, note.path.name))

    best_score = name_score
    best_excerpt: str | None = None
    best_line_number: int | None = None

    raw_lines = note.path.read_text(encoding="utf-8").splitlines()
    for i, line in enumerate(raw_lines):
        stripped = line.strip()
        if not stripped:
            continue
        score = int(fuzz.partial_ratio(query, stripped))
        if score > best_score:
            best_score = score
            best_excerpt = stripped
            best_line_number = i + 1

    return SearchResult(
        note=note,
        score=best_score,
        excerpt=best_excerpt,
        line_number=best_line_number,
    )

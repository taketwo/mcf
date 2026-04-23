from __future__ import annotations

import re
from dataclasses import dataclass
from typing import TYPE_CHECKING

from .notes import ParseError, enumerate_notes, load_note, slugify
from .schema import Kind

if TYPE_CHECKING:
    from collections.abc import Callable
    from pathlib import Path

    from .notes import Note


@dataclass
class LintViolation:
    """A lint rule violation found in a note."""

    path: Path
    message: str

    def __str__(self) -> str:
        """Return a human-readable violation message."""
        return f"{self.path}: {self.message}"


def check_filename_kind(note: Note) -> LintViolation | None:
    """Check that the kind suffix in the filename matches the frontmatter kind."""
    name = note.path.name
    stem = name[:-3]  # strip .md

    if note.meta.kind == Kind.OTHER:
        # No suffix expected — verify there's no kind-like suffix
        parts = stem.split("-")
        if len(parts) > 1:
            try:
                Kind(parts[-1])
            except ValueError:
                pass
            else:
                return LintViolation(
                    path=note.path,
                    message=f"kind is 'other' but filename has a kind suffix: {name}",
                )
    else:
        expected_suffix = f"-{note.meta.kind.value}"
        if not stem.endswith(expected_suffix):
            return LintViolation(
                path=note.path,
                message=f"filename kind suffix does not match frontmatter kind '{note.meta.kind.value}': {name}",
            )
    return None


def check_filename_slug(note: Note) -> LintViolation | None:
    """Check that the slug in the filename matches a slugified form of the note name."""
    expected_slug = slugify(note.meta.name)
    if note.slug != expected_slug:
        return LintViolation(
            path=note.path,
            message=f"filename slug '{note.slug}' does not match expected slug '{expected_slug}' (from name '{note.meta.name}')",
        )
    return None


_LIST_ITEM_RE = re.compile(r"^[-*+]\s|^\d+[.)]\s")


def _looks_wrapped(paragraph: list[str]) -> bool:
    """Return True if a paragraph looks like it was hard-wrapped."""
    if len(paragraph) < 3:  # noqa: PLR2004
        return False
    list_items = sum(1 for line in paragraph if _LIST_ITEM_RE.match(line))
    if list_items > len(paragraph) // 2:
        return False
    long_lines = sum(1 for line in paragraph if len(line) > 80)  # noqa: PLR2004
    short_lines = sum(1 for line in paragraph if 20 < len(line) <= 80)  # noqa: PLR2004
    return short_lines >= 3 and long_lines == 0  # noqa: PLR2004


def _count_wrapped_paragraphs(body_lines: list[str]) -> int:
    """Count paragraphs in body_lines that appear to be hard-wrapped."""
    in_code_block = False
    count = 0
    current: list[str] = []

    for line in body_lines:
        if line.startswith("```"):
            in_code_block = not in_code_block
            if not in_code_block and current:
                if _looks_wrapped(current):
                    count += 1
                current = []
            continue
        if in_code_block:
            continue
        if not line.strip():
            if current:
                if _looks_wrapped(current):
                    count += 1
                current = []
        else:
            current.append(line)

    if current and _looks_wrapped(current):
        count += 1
    return count


def check_no_line_wrapping(note: Note) -> LintViolation | None:
    """Check that prose paragraphs are not hard-wrapped at a short column width."""
    wrapped = _count_wrapped_paragraphs(note.body.splitlines())
    if wrapped > 0:
        return LintViolation(
            path=note.path,
            message=f"body appears to have {wrapped} hard-wrapped paragraph(s); write prose as one line per paragraph",
        )
    return None


def check_heading_case(note: Note) -> LintViolation | None:
    """Check that all headings use sentence case (only first word capitalised).

    Skips the first H1, which is tool-generated and follows its own convention.
    """
    lines = note.body.splitlines()
    in_code_block = False
    violations: list[str] = []
    first_h1_seen = False

    for line in lines:
        if line.startswith("```"):
            in_code_block = not in_code_block
            continue
        if in_code_block:
            continue
        if not line.startswith("#"):
            continue

        if line.startswith("# ") and not first_h1_seen:
            first_h1_seen = True
            continue

        heading_text = line.lstrip("#").strip()
        if not heading_text:
            continue

        # Strip leading numeric prefix (e.g. "2. " or "13) ") so numbered
        # headings don't false-positive on the first real word.
        heading_text = re.sub(r"^\d+[.)]\s+", "", heading_text)
        words = heading_text.split()
        # Heuristic: skip all-caps words (acronyms) and words with non-alpha chars.
        # Expect false positives on proper nouns (React, Docker, Kubernetes, etc.).
        bad_words = [
            w for w in words[1:] if w[0].isupper() and w.isalpha() and not w.isupper()
        ]
        if bad_words:
            violations.append(f"  {line!r} — capitalised words: {bad_words}")

    if violations:
        joined = "\n".join(violations)
        return LintViolation(
            path=note.path,
            message=f"headings should use sentence case:\n{joined}",
        )
    return None


RULES: list[Callable[[Note], LintViolation | None]] = [
    check_filename_kind,
    check_filename_slug,
    check_no_line_wrapping,
    check_heading_case,
]


def lint_note(path: Path) -> list[LintViolation]:
    """Parse note and run all registered rules. Parse failures are reported as violations."""
    try:
        note = load_note(path)
    except ParseError as e:
        return [LintViolation(path=path, message=f"parse error: {e.message}")]
    return [v for rule_fn in RULES if (v := rule_fn(note)) is not None]


def lint_path(target: Path) -> list[LintViolation]:
    """Lint a single file or recursively lint a directory."""
    if target.is_file():
        return lint_note(target)
    return [v for path in enumerate_notes(target) for v in lint_note(path)]

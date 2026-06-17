from __future__ import annotations

import re
from dataclasses import dataclass, field
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
    rule: str
    message: str
    line: int | None = field(default=None)

    def __str__(self) -> str:
        """Return a human-readable violation message."""
        if self.line is not None:
            return f"{self.path}:{self.line}: [{self.rule}] {self.message}"
        return f"{self.path}: [{self.rule}] {self.message}"


def check_filename_kind(note: Note) -> list[LintViolation]:
    """Check that the kind suffix in the filename matches the frontmatter kind."""
    name = note.path.name
    stem = name[:-3]  # strip .md

    if note.meta.kind == Kind.OTHER:
        parts = stem.split("-")
        if len(parts) > 1:
            try:
                Kind(parts[-1])
            except ValueError:
                pass
            else:
                return [
                    LintViolation(
                        path=note.path,
                        rule="filename-kind",
                        message=f"kind is 'other' but filename has a kind suffix: {name}",
                    )
                ]
    else:
        expected_suffix = f"-{note.meta.kind.value}"
        if not stem.endswith(expected_suffix):
            return [
                LintViolation(
                    path=note.path,
                    rule="filename-kind",
                    message=f"filename kind suffix does not match frontmatter kind '{note.meta.kind.value}': {name}",
                )
            ]
    return []


def check_filename_slug(note: Note) -> list[LintViolation]:
    """Check that the slug in the filename matches a slugified form of the note name."""
    expected_slug = slugify(note.meta.name)
    if note.slug != expected_slug:
        return [
            LintViolation(
                path=note.path,
                rule="filename-slug",
                message=f"filename slug '{note.slug}' does not match expected slug '{expected_slug}' (from name '{note.meta.name}')",
            )
        ]
    return []


_LIST_ITEM_RE = re.compile(r"^[-*+]\s|^\d+[.)]\s")


def _looks_wrapped(paragraph: list[str]) -> bool:
    """Return True if a paragraph is hard-wrapped (multiple lines, not a list or table)."""
    if len(paragraph) < 2:  # noqa: PLR2004
        return False
    list_items = sum(1 for line in paragraph if _LIST_ITEM_RE.match(line))
    if list_items > len(paragraph) // 2:
        return False
    table_rows = sum(1 for line in paragraph if line.startswith("|"))
    return table_rows <= len(paragraph) // 2


def _flush(current: list[str], start: int, results: list[int]) -> None:
    if current and _looks_wrapped(current):
        results.append(start)


def _is_indented_code_line(line: str) -> bool:
    """Return True if the line belongs to a markdown indented code block."""
    return bool(line) and (line.startswith("    ") or line.startswith("\t"))


def _find_wrapped_paragraphs(body_lines: list[str]) -> list[int]:
    """Return 1-based line numbers of the first line of each hard-wrapped paragraph."""
    in_code_block = False
    in_indented_code = False
    results: list[int] = []
    current: list[str] = []
    current_start: int = 0

    for i, line in enumerate(body_lines, start=1):
        if line.startswith("```"):
            in_indented_code = False
            in_code_block = not in_code_block
            if not in_code_block:
                _flush(current, current_start, results)
                current = []
            continue
        if in_code_block:
            continue
        if _is_indented_code_line(line):
            _flush(current, current_start, results)
            current = []
            in_indented_code = True
            continue
        if in_indented_code:
            if not line.strip():
                continue
            in_indented_code = False
        if not line.strip():
            _flush(current, current_start, results)
            current = []
        else:
            if not current:
                current_start = i
            current.append(line)

    _flush(current, current_start, results)
    return results


def check_no_line_wrapping(note: Note) -> list[LintViolation]:
    """Check that prose paragraphs are not hard-wrapped at a short column width."""
    wrapped_starts = _find_wrapped_paragraphs(note.body.splitlines())
    return [
        LintViolation(
            path=note.path,
            rule="no-line-wrapping",
            message="hard-wrapped paragraph; write prose as one line per paragraph",
            line=start,
        )
        for start in wrapped_starts
    ]


def check_heading_case(note: Note) -> list[LintViolation]:
    """Check that all headings use sentence case (only first word capitalised).

    Skips the first H1, which is tool-generated and follows its own convention.
    """
    lines = note.body.splitlines()
    in_code_block = False
    violations: list[LintViolation] = []
    first_h1_seen = False

    for lineno, line in enumerate(lines, start=1):
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

        # Split on em dash — each part is treated as a separate sentence.
        # Strip leading enumeration prefix (e.g. "2. ", "13) ", "1.2 ", "1.2.3 ") from
        # each part so numbered headings don't false-positive on the first real word.
        parts = heading_text.split(" — ")
        bad_words = []
        for part in parts:
            stripped = re.sub(r"^[\d.]+[.)]\s+|^\d+\.\d[\d.]*\s+", "", part)
            words = stripped.split()
            # Heuristic: skip all-caps words (acronyms) and words with non-alpha chars.
            # Expect false positives on proper nouns (React, Docker, Kubernetes, etc.).
            bad_words += [
                w
                for w in words[1:]
                if w[0].isupper() and w.isalpha() and not w.isupper()
            ]
        if bad_words:
            violations.append(
                LintViolation(
                    path=note.path,
                    rule="heading-case",
                    message=f"{line!r} — capitalised words: {bad_words}",
                    line=lineno,
                )
            )

    return violations


def check_heading_blank_line(note: Note) -> list[LintViolation]:
    """Check that each heading is followed by a blank line."""
    lines = note.body.splitlines()
    in_code_block = False
    violations: list[LintViolation] = []

    for lineno, line in enumerate(lines, start=1):
        if line.startswith("```"):
            in_code_block = not in_code_block
            continue
        if in_code_block:
            continue
        if not line.startswith("#"):
            continue
        # Check the next line exists and is non-empty
        if lineno < len(lines) and lines[lineno].strip():
            violations.append(
                LintViolation(
                    path=note.path,
                    rule="heading-blank-line",
                    message=f"heading not followed by a blank line: {line!r}",
                    line=lineno,
                )
            )

    return violations


RULES: list[Callable[[Note], list[LintViolation]]] = [
    check_filename_kind,
    check_filename_slug,
    check_no_line_wrapping,
    check_heading_blank_line,
    check_heading_case,
]


def lint_note(path: Path) -> list[LintViolation]:
    """Parse note and run all registered rules. Parse failures are reported as violations."""
    try:
        note = load_note(path)
    except ParseError as e:
        return [
            LintViolation(path=path, rule="parse", message=f"parse error: {e.message}")
        ]
    return [v for rule_fn in RULES for v in rule_fn(note)]


def lint_path(target: Path) -> list[LintViolation]:
    """Lint a single file or recursively lint a directory."""
    if target.is_file():
        return lint_note(target)
    return [v for path in enumerate_notes(target) for v in lint_note(path)]

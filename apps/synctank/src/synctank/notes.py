from __future__ import annotations

import re
from dataclasses import dataclass, field
from datetime import date
from typing import TYPE_CHECKING, Any

import frontmatter
import yaml

from .schema import Kind, Status

if TYPE_CHECKING:
    from collections.abc import Iterator
    from pathlib import Path

_FILENAME_RE = re.compile(r"^(\d{3})-(.+)\.md$")


def _is_note_file(name: str) -> bool:
    return _FILENAME_RE.match(name) is not None


@dataclass
class ParseError(Exception):
    """Raised when a note file cannot be parsed."""

    path: Path
    message: str

    def __str__(self) -> str:
        """Return a human-readable error message."""
        return f"{self.path}: {self.message}"


@dataclass
class Frontmatter:
    """Frontmatter fields for a note."""

    name: str
    kind: Kind
    status: Status
    date: date
    related: list[str] = field(default_factory=list)
    extra: dict[str, Any] = field(default_factory=dict)


@dataclass
class Note:
    """A parsed note file."""

    path: Path
    index: int
    slug: str
    meta: Frontmatter
    body: str

    def to_dict(self, *, include_body: bool = False) -> dict[str, Any]:
        """Return a JSON-serializable dictionary representation."""
        d: dict[str, Any] = {
            "path": str(self.path),
            "index": self.index,
            "slug": self.slug,
            "kind": self.meta.kind.value,
            "status": self.meta.status.value,
            "name": self.meta.name,
            "date": self.meta.date.isoformat(),
            "related": self.meta.related,
            **self.meta.extra,
        }
        if include_body:
            d["body"] = self.body
        return d


def enumerate_notes(root: Path, *, recursive: bool = True) -> Iterator[Path]:
    """Yield paths of note files under root matching the NNN-*.md naming pattern.

    Searches recursively by default. Yields files in sorted order within each directory.
    """
    pattern = "**/*.md" if recursive else "*.md"
    for path in sorted(root.glob(pattern)):
        if _is_note_file(path.name):
            yield path


def load_note(path: Path) -> Note:
    """Parse a note file into a Note instance.

    Raises ParseError on malformed input.
    """
    try:
        post = frontmatter.loads(path.read_text(encoding="utf-8"))
    except Exception as e:
        raise ParseError(path=path, message=f"failed to parse frontmatter: {e}") from e

    raw = post.metadata
    body = post.content.strip()

    name = _require_str(raw, "name", path)
    kind = _require_enum(raw, "kind", Kind, path)
    status = _require_enum(raw, "status", Status, path)
    note_date = _require_date(raw, "date", path)
    related = _parse_related(raw.get("related", []), path)
    extra = {
        k: v
        for k, v in raw.items()
        if k not in {"name", "kind", "status", "date", "related"}
    }
    index, slug = _parse_filename(path)

    return Note(
        path=path,
        index=index,
        slug=slug,
        meta=Frontmatter(
            name=name,
            kind=kind,
            status=status,
            date=note_date,
            related=related,
            extra=extra,
        ),
        body=body,
    )


def slugify(name: str) -> str:
    """Convert a name to a lowercase hyphenated slug."""
    slug = name.lower()
    slug = re.sub(r"[^\w\s-]", "", slug)
    slug = re.sub(r"[\s_]+", "-", slug)
    slug = re.sub(r"-+", "-", slug)
    return slug.strip("-")


def next_index(existing_filenames: list[str]) -> int:
    """Return the next available index given a list of existing filenames.

    Parses the NNN- numeric prefix from each filename. Returns max+1, or 1 if none found.
    Ignores filenames that don't match the pattern.
    """
    indices = [
        int(m.group(1))
        for name in existing_filenames
        if (m := _FILENAME_RE.match(name))
    ]
    return max(indices, default=0) + 1


def build_filename(index: int, slug: str, kind: Kind) -> str:
    """Construct the note filename from index, slug, and kind."""
    if kind.has_filename_suffix:
        return f"{index:03d}-{slug}-{kind.value}.md"
    return f"{index:03d}-{slug}.md"


def write_note(target_dir: Path, meta: Frontmatter, body: str = "") -> Note:
    """Create a new note file. Returns the created Note."""
    target_dir.mkdir(parents=True, exist_ok=True)

    existing = [p.name for p in target_dir.iterdir() if _is_note_file(p.name)]
    index = next_index(existing)
    slug = slugify(meta.name)
    filename = build_filename(index, slug, meta.kind)
    path = target_dir / filename

    content = _assemble_content(meta, body)
    path.write_text(content, encoding="utf-8")
    return Note(path=path, index=index, slug=slug, meta=meta, body=body.strip())


def _assemble_content(meta: Frontmatter, body: str) -> str:
    """Assemble the complete file content: frontmatter + H1 + body."""
    fm = _build_frontmatter(meta)
    h1 = _build_h1(meta.name, meta.kind)
    parts = [fm, h1]
    if body.strip():
        parts.append(body.strip())
    return "\n\n".join(parts) + "\n"


def _build_frontmatter(meta: Frontmatter) -> str:
    """Build the YAML frontmatter block."""
    fields: dict[str, Any] = {
        "name": meta.name,
        "kind": meta.kind.value,
        "status": meta.status.value,
        "date": meta.date,
    }
    if meta.related:
        fields["related"] = list(meta.related)
    fields.update(meta.extra)
    yaml_text = yaml.safe_dump(
        fields, sort_keys=False, default_flow_style=False, allow_unicode=True
    )
    return f"---\n{yaml_text.rstrip()}\n---"


def _build_h1(name: str, kind: Kind) -> str:
    """Build the H1 heading from name and kind."""
    if kind.has_filename_suffix:
        kind_label = kind.value.replace("-", " ").title()
        return f"# {name} — {kind_label}"
    return f"# {name}"


def _parse_filename(path: Path) -> tuple[int, str]:
    """Extract index and slug from a note filename. Raises ParseError if unparseable.

    Kind suffix detection: if the last hyphen-separated segment is a known Kind value,
    strip it to recover the slug. Otherwise the whole body is the slug.
    """
    m = _FILENAME_RE.match(path.name)
    if not m:
        raise ParseError(
            path=path, message=f"filename does not match expected pattern: {path.name}"
        )
    index = int(m.group(1))
    body = m.group(2)

    # Guess the kind suffix by matching against known Kind values (longest first to
    # avoid prefix ambiguity). This is a heuristic — cross-validation happens later
    # in check_filename_kind, which compares the guessed slug against frontmatter.
    for kind in sorted(Kind, key=lambda k: len(k.value), reverse=True):
        suffix = f"-{kind.value}"
        if body.endswith(suffix):
            return index, body[: -len(suffix)]

    return index, body


def _require_str(meta: dict[str, Any], key: str, path: Path) -> str:
    """Extract a required string field from frontmatter metadata."""
    value = meta.get(key)
    if not isinstance(value, str) or not value.strip():
        raise ParseError(
            path=path, message=f"missing or invalid required field: '{key}'"
        )
    return value


def _require_enum[E: (Kind, Status)](
    meta: dict[str, Any], key: str, enum_class: type[E], path: Path
) -> E:
    """Extract a required enum field from frontmatter metadata."""
    raw = meta.get(key)
    try:
        return enum_class(raw)
    except (ValueError, KeyError) as e:
        valid = [member.value for member in enum_class]
        raise ParseError(
            path=path,
            message=f"invalid value for '{key}': {raw!r}. Must be one of: {valid}",
        ) from e


def _require_date(meta: dict[str, Any], key: str, path: Path) -> date:
    """Extract a required date field from frontmatter metadata."""
    value = meta.get(key)
    if isinstance(value, date):
        return value
    if isinstance(value, str):
        try:
            return date.fromisoformat(value)
        except ValueError:
            pass
    raise ParseError(
        path=path,
        message=f"missing or invalid required field: '{key}' (expected YYYY-MM-DD)",
    )


def _parse_related(value: object, path: Path) -> list[str]:
    """Parse the optional related field into a list of strings."""
    if not value:
        return []
    if isinstance(value, list):
        result: list[str] = []
        for item in value:
            if not isinstance(item, str):
                raise ParseError(
                    path=path,
                    message=f"'related' entries must be strings, got: {item!r}",
                )
            result.append(item)
        return result
    raise ParseError(
        path=path, message=f"'related' must be a list, got: {type(value).__name__}"
    )

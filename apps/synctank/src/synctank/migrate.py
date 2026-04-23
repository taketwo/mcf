from __future__ import annotations

import re
from dataclasses import dataclass, field
from datetime import UTC, date, datetime
from typing import TYPE_CHECKING, Any

import frontmatter
import yaml

from .notes import Frontmatter, _assemble_content, build_filename, slugify
from .schema import Kind, Status

if TYPE_CHECKING:
    from pathlib import Path

_NUMERIC_PREFIX_RE = re.compile(r"^(\d+)-")

_KIND_KEYWORDS: list[tuple[list[str], Kind]] = [
    (["lessons learned", "lessons-learned"], Kind.LESSONS_LEARNED),
    (["system context", "brief", "overview", "context"], Kind.BRIEF),
    (["specification", "spec"], Kind.SPEC),
    (["design", "architecture"], Kind.DESIGN),
    (["planning", "plan"], Kind.PLAN),
    (["requirements"], Kind.REQUIREMENTS),
    (
        [
            "characterization",
            "findings",
            "analysis",
            "implementation",
            "progress",
            "insights",
            "report",
        ],
        Kind.REPORT,
    ),
    (["howto", "how to", "guide"], Kind.GUIDE),
    (["glossary", "checklist", "reference"], Kind.REFERENCE),
    (["brainstorm", "exploration", "ideas"], Kind.BRAINSTORM),
]

_CONSUMED_KEYS = frozenset(
    {"name", "title", "kind", "status", "date", "related", "parent", "based-on"}
)


@dataclass
class MigrateAction:
    """Planned action for a single file."""

    path: Path
    target_name: str  # new filename (may equal path.name if unchanged)
    new_content: str
    rewritten: bool
    mtime_ordered: bool = False  # True if index was assigned from mtime fallback

    @property
    def renamed(self) -> bool:
        """Return True if the file needs to be renamed."""
        return self.path.name != self.target_name

    @property
    def skipped(self) -> bool:
        """Return True if the file is already compliant and needs no changes."""
        return not self.renamed and not self.rewritten


@dataclass
class MigrateError:
    """A file that cannot be migrated automatically."""

    path: Path
    message: str


@dataclass
class MigratePlan:
    """The full plan for a directory migration."""

    actions: list[MigrateAction] = field(default_factory=list)
    errors: list[MigrateError] = field(default_factory=list)


def infer_name(meta: dict[str, Any], path: Path) -> str:
    """Infer the note name from frontmatter or filename."""
    if isinstance(meta.get("name"), str) and meta["name"].strip():
        return meta["name"].strip()
    if isinstance(meta.get("title"), str) and meta["title"].strip():
        return meta["title"].strip()
    stem = path.stem
    m = _NUMERIC_PREFIX_RE.match(stem)
    if m:
        stem = stem[m.end() :]
    return stem.replace("-", " ").replace("_", " ").title()


def infer_kind(name: str) -> Kind:
    """Infer document kind by keyword matching against name."""
    lower = name.lower()
    for keywords, kind in _KIND_KEYWORDS:
        if any(kw in lower for kw in keywords):
            return kind
    return Kind.OTHER


def _merge_related(meta: dict[str, Any]) -> list[str]:
    """Merge parent, based-on, and related fields into a deduplicated list."""
    seen: set[str] = set()
    result: list[str] = []

    def add(val: object) -> None:
        if isinstance(val, str) and val.strip() and val not in seen:
            seen.add(val)
            result.append(val)

    for key in ("parent", "based-on", "related"):
        val = meta.get(key)
        if isinstance(val, list):
            for item in val:
                add(item)
        else:
            add(val)

    return result


def _strip_leading_h1(body: str) -> str:
    """Remove the first H1 heading line from body, if present."""
    lines = body.splitlines()
    for i, line in enumerate(lines):
        if line.startswith("# "):
            return "\n".join(lines[i + 1 :]).strip()
        if line.strip():
            break
    return body


def _parse_file(text: str) -> tuple[dict[str, Any], str]:
    """Parse frontmatter and body from file text. Returns (meta, body).

    Files without frontmatter are returned with empty meta and full text as body.
    YAML parse errors are re-raised as ValueError.
    """
    try:
        post = frontmatter.loads(text)
    except yaml.YAMLError as e:
        raise ValueError(f"YAML parse error: {e}") from e

    if not post.metadata:
        return {}, _strip_leading_h1(text.strip())
    return dict(post.metadata), _strip_leading_h1(post.content.strip())


def _mtime_date(path: Path) -> date:
    """Return the file's modification time as a date in UTC."""
    return datetime.fromtimestamp(path.stat().st_mtime, tz=UTC).date()


def _sort_key(path: Path) -> tuple[int, float, str]:
    """Sort key: (has_prefix, numeric_prefix or mtime, name)."""
    m = _NUMERIC_PREFIX_RE.match(path.name)
    if m:
        return (0, float(m.group(1)), path.name)
    return (1, path.stat().st_mtime, path.name)


def plan_directory(directory: Path) -> MigratePlan:
    """Build a migration plan for all .md files in a single directory."""
    plan = MigratePlan()

    files = sorted(
        (p for p in directory.iterdir() if p.is_file() and p.suffix == ".md"),
        key=_sort_key,
    )

    for idx, path in enumerate(files, start=1):
        mtime_ordered = _NUMERIC_PREFIX_RE.match(path.name) is None

        try:
            original_text = path.read_text(encoding="utf-8")
            meta, body = _parse_file(original_text)
        except (ValueError, OSError) as e:
            plan.errors.append(MigrateError(path=path, message=str(e)))
            continue

        name = infer_name(meta, path)
        kind = infer_kind(name)
        _raw_date = meta.get("date")
        note_date: date = (
            _raw_date if isinstance(_raw_date, date) else _mtime_date(path)
        )
        related = _merge_related(meta)
        extra = {k: v for k, v in meta.items() if k not in _CONSUMED_KEYS}

        slug = slugify(name)
        target_name = build_filename(idx, slug, kind)
        params = Frontmatter(
            name=name,
            kind=kind,
            status=Status.DRAFT,
            date=note_date,
            related=related,
            extra=extra,
        )
        new_content = _assemble_content(params, body)
        plan.actions.append(
            MigrateAction(
                path=path,
                target_name=target_name,
                new_content=new_content,
                rewritten=original_text != new_content,
                mtime_ordered=mtime_ordered,
            )
        )

    return plan


def plan_migration(root: Path) -> dict[Path, MigratePlan]:
    """Build migration plans for root and all immediate subdirectories."""
    plans: dict[Path, MigratePlan] = {}

    dirs = [
        root,
        *sorted(d for d in root.iterdir() if d.is_dir() and not d.name.startswith(".")),
    ]
    for d in dirs:
        if any(p.is_file() and p.suffix == ".md" for p in d.iterdir()):
            plans[d] = plan_directory(d)

    return plans


def apply_plan(plan: MigratePlan) -> None:
    """Execute a migration plan, renaming and rewriting files."""
    for action in plan.actions:
        if action.skipped:
            continue
        target = action.path.parent / action.target_name
        action.path.write_text(action.new_content, encoding="utf-8")
        if action.renamed:
            action.path.rename(target)

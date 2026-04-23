from enum import StrEnum


class Kind(StrEnum):
    """Document kind vocabulary."""

    BRIEF = "brief"
    DESIGN = "design"
    REQUIREMENTS = "requirements"
    SPEC = "spec"
    PLAN = "plan"
    REPORT = "report"
    BRAINSTORM = "brainstorm"
    LESSONS_LEARNED = "lessons-learned"
    GUIDE = "guide"
    REFERENCE = "reference"
    OTHER = "other"

    @property
    def has_filename_suffix(self) -> bool:
        """Return True if this kind should appear as a suffix in the filename."""
        return self != Kind.OTHER

    @property
    def description(self) -> str:
        """Return the full human-readable description of this kind."""
        return _KIND_DESCRIPTIONS[self]


_KIND_DESCRIPTIONS: dict[Kind, str] = {
    Kind.BRIEF: (
        "High-level project overview: goals, scope, and motivation. "
        "The starting point for a new project or subproject. "
        "Not a design — contains no technical decisions."
    ),
    Kind.DESIGN: (
        "Records architectural or technical decisions and the reasoning behind them. "
        "The document answers 'why this approach' as much as 'what the approach is.' "
        "Use 'spec' instead if the document is purely prescriptive with no decision-making content."
    ),
    Kind.REQUIREMENTS: (
        "What the system must do; functional and non-functional constraints. "
        "Describes the problem space, not the solution."
    ),
    Kind.SPEC: (
        "Precise, prescriptive description of a behavior, interface, file format, or protocol. "
        "Tells the implementer exactly what to build. "
        "Narrower and more concrete than a design — no architectural deliberation, just the agreed outcome."
    ),
    Kind.PLAN: (
        "Ordered sequence of work to be done: tasks, milestones, or commit sequences. "
        "Forward-looking and actionable."
    ),
    Kind.REPORT: (
        "Findings or results from an investigation, experiment, or analysis run. "
        "Describes what was done and what was found."
    ),
    Kind.BRAINSTORM: (
        "Unstructured exploration of ideas, options, or approaches. "
        "Not expected to be authoritative or complete. "
        "A thinking-out-loud document."
    ),
    Kind.LESSONS_LEARNED: (
        "Retrospective observations from completed work: what worked, what didn't, what to carry forward. "
        "Written after the fact, not during."
    ),
    Kind.GUIDE: (
        "Normative instructional document addressed to the reader as actor. "
        "Explains how something should be done and gives concrete steps to follow. "
        "Different from 'spec' (which describes what a thing is) and 'design' (which records decisions)."
    ),
    Kind.REFERENCE: (
        "Stable lookup material: tables, glossaries, checklists, parameter sheets, "
        "or codebase documentation that may eventually be absorbed into the VCS. "
        "Updated when the underlying facts change, not as a record of activity."
    ),
    Kind.OTHER: "Doesn't fit any of the above. No kind suffix in filename.",
}


class Status(StrEnum):
    """Document status vocabulary."""

    DRAFT = "draft"
    LIVING = "living"
    COMPLETE = "complete"
    SUPERSEDED = "superseded"

    @property
    def description(self) -> str:
        """Return the full human-readable description of this status."""
        return _STATUS_DESCRIPTIONS[self]


_STATUS_DESCRIPTIONS: dict[Status, str] = {
    Status.DRAFT: (
        "Incomplete, not yet ready to act on. The normal starting state. "
        "Use this when the document will eventually reach a finished state — "
        "do not use 'living' just because the document might change during implementation."
    ),
    Status.LIVING: (
        "Permanently incomplete by design — continuously updated as the project evolves "
        "and never expected to be 'done.' "
        "Examples: a running log, a glossary, a lessons-learned document that accumulates entries over time. "
        "Do not use for documents that are simply in progress."
    ),
    Status.COMPLETE: (
        "No further updates expected; reflects a finished state. The document has served its purpose."
    ),
    Status.SUPERSEDED: "Replaced by a newer document; kept for historical reference only.",
}

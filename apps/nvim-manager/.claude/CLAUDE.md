# nvim-manager - Project Guidelines

## Stack

- Language: Python 3.13
- Package manager: `uv`
- Task runner: `just`
- Linter/formatter: `ruff`
- Type checker: `ty`
- Test framework: `pytest`

## Common Tasks

All important operations are wrapped in `just` recipes and pre-approved — always prefer them over raw commands.

- `just check` — run all checks (format, lint, type)
- `just fix` — auto-format and auto-fix linting issues
- `just test` — run tests with coverage
- `just test -k <filter>` — run tests matching filter
- `just run <args>` — run the CLI

Other common operations:

- `uv add <package>` — add runtime dependency
- `uv add --dev <package>` — add development dependency

## Development Workflow

When implementing changes, follow this sequence:

1. Implement the changes
2. `just fix` — auto-format and fix linting issues
3. `just check` — verify all checks pass
4. `just test` — verify all tests pass
5. `just run` — smoke-test the CLI if applicable
6. Hand back to the user for review

## Code Style

- Docstrings: NumPy style with Parameters, Returns, Raises sections as appropriate
- Type annotations: use `int | None` over `Optional[int]`, `list[str]` over `List[str]`, `Path` for filesystem paths
- Logging: `from .logging import get_logger`; use `%` formatting, not f-strings

## Code Quality

This project uses ruff with ALL diagnostics enabled (with a few exceptions defined in `pyproject.toml`).

Handling ruff diagnostics:
1. Try to fix diagnostics first — most diagnostics point to real issues or better patterns
2. Don't go crazy — if a diagnostic seems unreasonable for this project, consider ignoring it
3. Ignore hierarchy (in order of preference):
   - Line-level: `# noqa: <code>`
   - File-level: `# ruff: noqa: <code>` at top of file
   - Pattern-based: `lint.per-file-ignores` in `pyproject.toml`
   - Project-wide: `lint.ignore` in `pyproject.toml`
4. ALL ignores require explicit user permission — NEVER add any ignore without asking first

## Change Management

- NEVER commit without explicit instruction from the user
- NEVER use `git add -A` or `git add .` — stage specific files, or ask the user to stage
- Commit title format: `Nvim-manager: <short imperative description>`
- NEVER include attribution footers in commit messages

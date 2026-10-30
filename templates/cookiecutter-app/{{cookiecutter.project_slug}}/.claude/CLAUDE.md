# {{ cookiecutter.project_name }} - Project Guidelines

## Stack

- Language: Python {{ cookiecutter.python_version }}
- Package manager: `uv`
- Task runner: `just`
- Linter/formatter: `ruff`, plus `tombi` for formatting `pyproject.toml`
- Type checker: `ty`
- Test framework: `pytest`

## Common Tasks

All important operations are wrapped in `just` recipes and pre-approved — always prefer them over raw commands.

- `just setup` — set up the development environment; recipes run tools from `.venv` and need it to exist
- `just check` — run all checks (format, lint, type)
- `just fix` — auto-fix linting issues and auto-format
- `just test` — run tests with coverage
- `just test -k <filter>` — run tests matching filter
- `just run <args>` — run the CLI
- `just upgrade-checkers` — pin `ruff`, `tombi` and `ty` to their latest releases and reformat

Other common operations:

- `uv add <package>` — add runtime dependency
- `uv add --dev <package>` — add development dependency

`ruff`, `tombi` and `ty` are pinned exactly. Bump them only with `just upgrade-checkers`.

## Development Workflow

When implementing changes, follow this sequence:

1. Implement the changes
2. `just fix` — fix linting issues and auto-format
3. `just check` — verify all checks pass
4. `just test` — verify all tests pass
5. `just run` — smoke-test the CLI if applicable
6. Hand back to the user for review

## Code Quality

This project uses ruff with ALL diagnostics enabled (with a few exceptions defined in `pyproject.toml`).

Handling ruff diagnostics:
1. Try to fix diagnostics first — most diagnostics point to real issues or better patterns
2. Don't go crazy — if a diagnostic seems unreasonable for this project, consider ignoring it
3. Ignore hierarchy (in order of preference):
   - Line-level: `# noqa: <code>`
   - File-level: `# ruff: noqa: <code>` at top of file
   - Pattern-based: `[tool.ruff.lint.per-file-ignores]` in `pyproject.toml`
   - Project-wide: `ignore` in `[tool.ruff.lint]` in `pyproject.toml`
4. ALL ignores require explicit user permission — NEVER add any ignore without asking first

## Conventions

- Logs are diagnostics: progress, warnings, errors, debug output. They go through `get_logger` and always end up on stderr.
- Results go to stdout through `click.echo` or a rich console. Do not use `print`.
- Docstrings follow the numpy convention.
- Mark heavy imports with `lazy import x` or `lazy from x import y`, so that `--help` and completion stay fast. A missing dependency then fails on first use, not at startup.

## Change Management

- NEVER commit without explicit instruction from the user
- NEVER use `git add -A` or `git add .` — stage specific files, or ask the user to stage
- Commit title format: `{{ cookiecutter.commit_prefix }}: <short imperative description>`
- NEVER include attribution footers in commit messages

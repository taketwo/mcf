# ClaudeCode Configuration Management Design

## Overview

Add `ccu config` command group to manage alignment of `~/.claude.json` settings with desired state. This addresses the problem where some ClaudeCode configuration lives in `~/.claude.json` (cannot be version-controlled due to machine-specific ephemeral data) but certain settings like `autoCompactEnabled` should be consistent across all machines.

## Problem Statement

- `settings.json` is under version control and handles project-specific ClaudeCode config
- `~/.claude.json` contains ~32 fields mixing user preferences with machine-specific state
- Cannot version control `~/.claude.json` due to ephemeral fields (tokens, timestamps, etc.)
- Need a way to enforce desired values for specific fields across machines

## Solution

Create a new command group `ccu config` with two subcommands that manage alignment between current and desired state for specific `~/.claude.json` fields.

## Architecture

### File Organization

```
src/ccu/
├── commands/
│   └── config.py          # New: config command group
├── cli.py                 # Updated: register config group
└── ... (existing files)
```

### Desired Settings Storage

Hardcoded in `config.py` as Python dict (similar to existing plugins reference):

```python
DESIRED_SETTINGS = {
    "autoCompactEnabled": False,
    # Add more settings as needed
}
```

## Commands

### `ccu config check`

**Purpose:** Report alignment status without making changes.

**Behavior:**
1. Read and parse `~/.claude.json`
2. Compare each desired setting against current value
3. Display symbol-based output (matching existing plugins style):
   ```
   ~/.claude.json settings:
     ✓ autoCompactEnabled: false
     ✗ someOtherSetting: true
   ```
   - ✓ = current value matches desired
   - ✗ = current value differs from desired (or doesn't exist)

**Exit Codes:**
- `0` - All settings aligned
- `1` - One or more settings misaligned
- `2` - Error (file not found, JSON parse error, read error)

**Output:**
- Status information to stdout
- Errors to stderr

### `ccu config apply`

**Purpose:** Enforce desired settings.

**Behavior:**
1. Read and parse `~/.claude.json`
2. Compare desired settings against current values
3. If all aligned:
   - Print success message
   - Exit 0 (no file write)
4. If misaligned:
   - Copy `~/.claude.json` to `~/.claude.json.bak` (overwrites existing backup)
   - Update parsed JSON with desired values (adds missing fields)
   - Write modified JSON back to `~/.claude.json`
   - Display results:
     ```
     ~/.claude.json settings:
       ✓ autoCompactEnabled: false (already correct)
       ✚ someOtherSetting: false (updated)
     ```
     - ✓ = was already correct
     - ✚ = was updated or added

**Exit Codes:**
- `0` - Success (settings applied or already aligned)
- `1` - Write operation failed
- `2` - Error (file not found, JSON parse error, read/backup error)

**File Handling:**
- Uses standard `json.load()` / `json.dump()`
- Does not enforce specific formatting (preserves ClaudeCode's preferences)
- Only writes if changes needed
- Creates single `.bak` file (overwrites previous backup)

## Implementation Details

### Module Structure (`src/ccu/commands/config.py`)

```python
"""ClaudeCode configuration management."""
import json
import shutil
from pathlib import Path
import click
from ccu.logging import get_logger

DESIRED_SETTINGS = {
    "autoCompactEnabled": False,
    # Add more settings here as needed
}

CLAUDE_JSON_PATH = Path.home() / ".claude.json"

@click.group()
def config() -> None:
    """Manage ClaudeCode configuration."""
    pass

@config.command()
def check() -> None:
    """Check alignment of ~/.claude.json with desired state."""
    # Implementation

@config.command()
def apply() -> None:
    """Apply desired settings to ~/.claude.json."""
    # Implementation
```

### Error Handling

- Wrap all file I/O in try/except blocks
- Catch specific exceptions: `FileNotFoundError`, `json.JSONDecodeError`, `OSError`
- Log detailed errors via `ccu.logging.get_logger()`
- Print user-friendly error messages to stderr
- Return appropriate exit codes

### Logging

- Use existing `ccu.logging.get_logger(__name__)` pattern
- Debug-level logging for operations
- Error-level logging for failures
- Follow existing patterns from hooks and plugins commands

### CLI Integration (`cli.py`)

```python
from ccu.commands.config import config

# Register command group
main.add_command(config)
```

## Design Decisions

### Why separate check/apply commands?

More explicit than single command with `--dry-run` flag. Clear intent, easier to script.

### Why hardcode desired settings in Python?

- Simpler than external config file for initial implementation
- Follows existing pattern from plugins reference config
- Easy to version control as part of codebase
- Can be extracted to config file later if needed

### Why single .bak file instead of timestamped?

- Simpler, doesn't accumulate files
- User typically only cares about "before last change"
- Can manually copy backup if preservation needed

### Why preserve ClaudeCode's JSON formatting?

- ClaudeCode frequently rewrites `~/.claude.json`
- Attempting to enforce our own formatting would cause constant churn
- Let ClaudeCode manage its own file format

### Why skip write when aligned?

- Minimizes file modification timestamps
- Avoids potential race conditions with ClaudeCode
- Clearer user feedback (no-op vs actual change)

## Future Enhancements

- Extract desired settings to external config file if list grows large
- Add `--verbose` flag to show all fields in `~/.claude.json`, not just managed ones
- Add validation for setting values (type checking, allowed values)
- Support removing fields (currently only adds/updates)

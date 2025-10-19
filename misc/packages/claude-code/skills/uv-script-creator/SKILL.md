---
name: uv-script-creator
description: Use when creating standalone executable Python scripts with dependencies. This skill applies when users request scripts that should run independently with their own dependency management, using Python's inline script metadata format (PEP 723).
---

# UV Script Creator

## Overview

This skill guides creation of standalone Python scripts using Python's inline script metadata format (PEP 723). These scripts are self-contained, declaring their dependencies inline using standardized metadata, and are managed automatically by uv without requiring manual virtual environment setup.

## When to Use This Skill

Use this skill when users request:
- "Create a standalone Python script..."
- "Write an executable Python script..."
- "Make a Python script that uses [library]..."
- Any request for a Python script meant to run independently with its own dependencies

## Workflow

### 1. Confirm Script Name

Infer an appropriate script name from the user's request and confirm with the user.

**Guidelines:**
- Use descriptive names based on the script's purpose (e.g., `fetch_github_data.py`, `process_csv.py`)
- Default to `.py` extension for clarity
- Drop the `.py` extension only if user explicitly requests it
- Use snake_case for Python script names

**Example:**
- User: "Create a script to fetch GitHub repository data"
- Infer: `fetch_github_data.py`
- Ask: "I'll create `fetch_github_data.py`. Is this name okay, or would you prefer a different name?"

### 2. Initialize the Script

Use `uv init --script` to create the script with inline metadata structure:

```bash
uv init --script <script_name>.py
```

**Python version handling:**
- Omit `--python` flag by default (uses system default Python)
- Only specify `--python <version>` if user explicitly requests a specific Python version
- Example with version: `uv init --script example.py --python 3.12`

### 3. Make Script Executable

Immediately after initialization, add shebang and set executable permissions:

**Add shebang as the first line:**
```python
#!/usr/bin/env -S uv run --script
# /// script
# dependencies = []
# ///

# Your code here
```

**Make executable:**
```bash
chmod +x <script_name>.py
```

After this, the script can be run directly:
```bash
./<script_name>.py
```

Or with `uv run`:
```bash
uv run <script_name>.py
```

### 4. Write the Script Code

Implement the requested functionality. The script now has the inline metadata block and shebang already present:

```python
#!/usr/bin/env -S uv run --script
# /// script
# dependencies = []
# ///

# Your code here
```

### 5. Add Dependencies as Needed

As dependencies are introduced in the code, use `uv add --script` to declare them:

```bash
uv add --script <script_name>.py <package> [<package2> ...]
```

**Examples:**
```bash
uv add --script fetch_data.py requests
uv add --script process_data.py 'pandas>=2.0' 'numpy<2'
uv add --script example.py rich httpx
```

This automatically updates the inline metadata block:

```python
# /// script
# dependencies = [
#   "requests",
#   "pandas>=2.0",
#   "numpy<2",
# ]
# ///
```

## Optional Features

### Locking Dependencies

Dependencies can be locked for reproducibility using:

```bash
uv lock --script <script_name>.py
```

This creates a `.lock` file (e.g., `example.py.lock`) adjacent to the script.

**When to use:**
- User explicitly requests dependency locking
- Script needs reproducible builds across environments
- Long-term maintenance is expected

**Do not lock by default** unless requested.

### Pinning to Historical Package Versions

For improved reproducibility, the `exclude-newer` field can limit uv to only consider distributions released before a specific date:

```python
# /// script
# dependencies = [
#   "requests",
# ]
# [tool.uv]
# exclude-newer = "2023-10-16T00:00:00Z"
# ///
```

The date must be in RFC 3339 format (e.g., `2006-12-02T02:07:43Z`).

**When to use:**
- User explicitly requests version pinning to a date
- Maximum reproducibility is required
- Avoiding future package updates is desired

**Do not use by default** unless requested.

### Specifying Python Version in Metadata

Python version requirements can be declared in the inline metadata:

```python
# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///
```

**When to specify:**
- User explicitly requests specific Python version
- Code uses syntax only available in certain Python versions
- Compatibility requirements are known

**Default behavior:** Omit `requires-python` to allow any compatible Python version.

## Complete Example

User request: "Create a standalone script to fetch and pretty-print GitHub repository data"

**Workflow:**

1. Confirm name: "I'll create `fetch_github_data.py`. Is this okay?"
2. Initialize: `uv init --script fetch_github_data.py`
3. Add shebang and make executable: `chmod +x fetch_github_data.py`
4. Write code and add dependencies: `uv add --script fetch_github_data.py requests rich`

**Final script:**

```python
#!/usr/bin/env -S uv run --script
# /// script
# dependencies = [
#   "requests",
#   "rich",
# ]
# ///

import sys
import requests
from rich.pretty import pprint

def fetch_repo_data(owner, repo):
    url = f"https://api.github.com/repos/{owner}/{repo}"
    response = requests.get(url)
    response.raise_for_status()
    return response.json()

if __name__ == "__main__":
    if len(sys.argv) != 3:
        print("Usage: ./fetch_github_data.py <owner> <repo>")
        sys.exit(1)

    owner, repo = sys.argv[1], sys.argv[2]
    data = fetch_repo_data(owner, repo)
    pprint({
        "name": data["name"],
        "description": data["description"],
        "stars": data["stargazers_count"],
        "forks": data["forks_count"]
    })
```

**Usage:**
```bash
./fetch_github_data.py astral-sh uv
```

## Key Principles

1. **Always use inline metadata** - Never suggest manual virtual environment creation or requirements.txt for standalone scripts
2. **Confirm script names** - Infer from context but let user confirm or provide alternative
3. **Default to simplicity** - Use system Python, omit optional features unless requested
4. **Make executable by default** - Add shebang and chmod +x for standalone scripts
5. **Use `uv add --script`** - Manage dependencies declaratively as they're introduced
6. **Keep .py extension** - Use `.py` unless user explicitly wants to drop it

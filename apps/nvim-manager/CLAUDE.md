# nvim-manager Development Guide

## Code Style Conventions

### Docstrings
- Use NumPy docstring style throughout the project
- Include Parameters, Returns, Raises sections as appropriate
- Example format:
```python
def function_name(param1: str, param2: int) -> bool:
    """Brief description of the function.

    Longer description if needed, explaining the purpose
    and behavior of the function.

    Parameters
    ----------
    param1 : str
        Description of the first parameter.
    param2 : int
        Description of the second parameter.

    Returns
    -------
    bool
        Description of the return value.

    Raises
    ------
    ValueError
        When parameter validation fails.
    FileNotFoundError
        When required files are missing.
    """
```

### Type Annotations
- Use modern type annotations throughout (mypy compatible)
- Use `int | None` instead of `Optional[int]` or `Union[int, None]`
- Use `list[str]` instead of `List[str]`, `dict[str, any]` instead of `Dict[str, Any]`
- Only import from `typing` for complex types not available as built-ins
- Use `Path` for file system paths

### Code Formatting
- Use `black` with line length 88

### Testing
- Use pytest for testing
- Run tests with `uv run pytest`
- Strategic test coverage focusing on core workflows

### Logging
- Import logger with `from .logging import get_logger` and create module logger: `logger = get_logger(__name__)`
- Use structured logging with % formatting, not f-strings

### Linting
- Use Ruff for fast Python linting and formatting checks
- Use mypy for type checking

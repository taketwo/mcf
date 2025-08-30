"""Language checks for files.

This module allows to check whether a file belongs to a specific programming language
based on extensions and shebang patterns.
"""

from pathlib import Path
from typing import Any

from ccu.logging import get_logger

logger = get_logger(__name__)

LANGUAGE_DEFINITIONS: dict[str, dict[str, Any]] = {
    "python": {
        "extensions": [".py", ".pyw"],
        "shebang_patterns": ["python", "uv run --script"],
    },
}


def is_file_in_language(file_path: Path, language: str) -> bool:
    """Check if file matches the specified language.

    Parameters
    ----------
    file_path : Path
        The file path to check.
    language : str
        The language to check against (e.g., "python").

    Returns
    -------
    bool
        True if the file matches the language, False otherwise.

    """
    if language not in LANGUAGE_DEFINITIONS:
        logger.warning("Unknown language: '%s'", language)
        return False

    definitions = LANGUAGE_DEFINITIONS[language]

    # Fast path: check extension first
    if file_path.suffix in definitions["extensions"]:
        logger.debug(
            "File %s matches '%s' via extension %s",
            file_path.name,
            language,
            file_path.suffix,
        )
        return True

    # For extensionless files, check shebang patterns
    if not file_path.suffix and file_path.exists():
        try:
            with file_path.open(encoding="utf-8", errors="ignore") as f:
                first_line = f.readline().rstrip("\n\r")
            for pattern in definitions["shebang_patterns"]:
                if pattern in first_line:
                    logger.debug(
                        "File %s matches '%s' via shebang pattern '%s'",
                        file_path.name,
                        language,
                        pattern,
                    )
                    return True
        except (OSError, IndexError) as e:
            logger.debug(
                "Could not read file %s for shebang check: %s",
                file_path.name,
                e,
            )

    logger.debug("File %s does not match language '%s'", file_path.name, language)
    return False

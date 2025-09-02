"""Project detection and context management for CCU.

This module provides project detection functionality to determine workspace
boundaries and project characteristics for tool execution.
"""

import os
from pathlib import Path

from ccu.logging import get_logger

logger = get_logger(__name__)


class Project:
    """Represents a project workspace with lazy property detection.

    A project is defined by the Claude workspace directory (CLAUDE_PROJECT_DIR)
    and provides methods to detect project characteristics and file membership.
    """

    def __init__(self, root: Path) -> None:
        """Initialize project with root directory.

        Parameters
        ----------
        root : Path
            The project root directory path.

        """
        self.root = root.resolve()
        self._has_python: bool | None = None

    @property
    def has_python(self) -> bool:
        """Check if this is a Python project (lazy evaluation).

        Returns
        -------
        bool
            True if Python project indicators are found.

        """
        if self._has_python is None:
            python_indicators = [
                self.root / "pyproject.toml",
                self.root / "setup.py",
                self.root / "requirements.txt",
            ]
            self._has_python = any(
                indicator.exists() for indicator in python_indicators
            )
            logger.debug(
                "Project %s has_python: %s",
                self.root,
                self._has_python,
            )
        return self._has_python

    def is_file_in_project(self, file_path: str) -> bool:
        """Check if a file is within project boundaries.

        Parameters
        ----------
        file_path : str
            The file path to check.

        Returns
        -------
        bool
            True if file is within project boundaries.

        """
        try:
            resolved_file = Path(file_path).resolve()
            resolved_file.relative_to(self.root)
        except ValueError:
            logger.debug("File %s is outside project %s", file_path, self.root)
            return False
        else:
            logger.debug("File %s is in project %s", file_path, self.root)
            return True


def get_project() -> Project:
    """Get the current project workspace.

    Uses CLAUDE_PROJECT_DIR environment variable to determine project root, falling
    back to current directory.

    Returns
    -------
    Project
        The current project instance.

    """
    project_path = Path(os.environ.get("CLAUDE_PROJECT_DIR", ".")).resolve()
    logger.debug("Getting project from: %s", project_path)
    return Project(project_path)

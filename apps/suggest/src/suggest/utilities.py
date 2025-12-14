"""Utility functions for the suggest tool.

This module contains helper functions for system information,
file operations, and other common tasks.
"""

import contextlib
import json
import os
import platform
from pathlib import Path
from typing import Any

from jinja2 import Template

from .logging import get_logger

logger = get_logger(__name__)


class PromptTemplateNotFoundError(FileNotFoundError):
    """Raised when a prompt template file is not found."""


class JSONParseError(ValueError):
    """Raised when JSON response cannot be parsed."""


def get_system_info() -> dict[str, str]:
    """Get detailed system information.

    Returns
    -------
    dict[str, str]
        Dictionary containing:
        - distro: Linux distribution name and version
        - shell: Current shell name
        - cwd: Current working directory

    """
    info = {}

    # Get Linux distribution info
    try:
        system = platform.system()
        if system == "Linux":
            try:
                import distro  # noqa: PLC0415

                info["distro"] = f"{distro.name()} {distro.version()}".strip()
            except ImportError:
                logger.warning("distro package not available, using fallback")
                # Fallback if distro package not available
                info["distro"] = "Linux"
        else:
            info["distro"] = system
    except (OSError, AttributeError) as e:
        logger.warning("Failed to get system info: %s", e)
        info["distro"] = "Linux"

    # Get shell from SHELL environment variable
    shell_path = os.environ.get("SHELL", "/bin/bash")
    info["shell"] = Path(shell_path).name

    # Get current working directory
    info["cwd"] = str(Path.cwd())

    logger.debug("Gathered system info: %s", info)
    return info


def load_prompt_template(template_name: str) -> str:
    """Load a Jinja2 prompt template from the prompts directory.

    Parameters
    ----------
    template_name : str
        Name of the template file (without .jinja extension).

    Returns
    -------
    str
        The template content.

    Raises
    ------
    PromptTemplateNotFoundError
        If the template file doesn't exist.

    """
    template_path = Path(__file__).parent / "prompts" / f"{template_name}.jinja"

    if not template_path.exists():
        logger.error("Prompt template not found: %s", template_name)
        msg = f"Prompt template {template_name} not found"
        raise PromptTemplateNotFoundError(msg)

    logger.debug("Loading prompt template: %s", template_name)
    return template_path.read_text(encoding="utf-8")


def render_prompt(template_content: str, **kwargs: object) -> str:
    """Render a Jinja2 template with the given variables.

    Parameters
    ----------
    template_content : str
        The template content to render.
    **kwargs : object
        Variables to pass to the template.

    Returns
    -------
    str
        The rendered template.

    """
    template = Template(template_content)
    return template.render(**kwargs)


def parse_json_response(response_text: str) -> dict[str, Any]:
    """Parse JSON response from LLM, handling potential formatting issues.

    Parameters
    ----------
    response_text : str
        Raw text response from the LLM.

    Returns
    -------
    dict[str, Any]
        Parsed JSON data.

    Raises
    ------
    JSONParseError
        If the response cannot be parsed as JSON.

    """
    logger.debug("Parsing JSON response (length: %d chars)", len(response_text))

    # Clean up the response text
    cleaned_text = response_text.strip()

    # Remove markdown code blocks if present
    if cleaned_text.startswith("```json") and cleaned_text.endswith("```"):
        logger.debug("Removing JSON markdown code blocks")
        cleaned_text = cleaned_text[7:-3].strip()
    elif cleaned_text.startswith("```") and cleaned_text.endswith("```"):
        logger.debug("Removing generic markdown code blocks")
        cleaned_text = cleaned_text[3:-3].strip()

    # Try to parse as JSON
    result = None
    try:
        result = json.loads(cleaned_text)
        logger.debug("Successfully parsed JSON on first attempt")
    except json.JSONDecodeError as e:
        logger.warning("Initial JSON parse failed: %s", e)
        # Try to extract JSON from the text - look for the first { and last }
        start = cleaned_text.find("{")
        end = cleaned_text.rfind("}")

        if start != -1 and end != -1 and end > start:
            json_part = cleaned_text[start : end + 1]
            logger.debug(
                "Attempting to extract JSON substring from position %d to %d",
                start,
                end,
            )
            with contextlib.suppress(json.JSONDecodeError):
                result = json.loads(json_part)
                logger.info("Successfully parsed JSON after extraction")

        if result is None:
            logger.exception("Failed to parse JSON response after all attempts")
            msg = f"Could not parse JSON response: {e}"
            raise JSONParseError(msg) from e

    # Validate that result is a dict
    if not isinstance(result, dict):
        logger.error("Parsed JSON is not a dictionary, got type: %s", type(result))
        msg = "Expected JSON object, got different type"
        raise JSONParseError(msg)

    return result

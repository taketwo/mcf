"""Mock LLM Client for testing and UX development.

This module provides a mock implementation of the LLM client interface
that returns predefined responses for testing UX flows without making
actual LLM calls.

Mock scenarios are triggered by specific request strings:
- "default" (or any other text): Normal successful command
- "error": Raises LLMGenerationError
- "low confidence": Returns command with confidence < 0.5
- "explanation error": Sets flag to make next explain_command() fail
- "revision error": Sets flag to make next revision fail
"""

import asyncio
from collections.abc import Callable
from typing import Any

from .llm_client import LLMExplanationError, LLMGenerationError


class MockLLMClient:
    """Mock LLM client for testing and UX development.

    This class implements the same interface as LLMClient but returns
    predefined responses based on scenario keywords in the request.
    """

    def __init__(self, model_name: str = "mock") -> None:
        """Initialize the mock LLM client.

        Parameters
        ----------
        model_name : str, optional
            Mock model name (default: "mock").

        """
        self.model_name = model_name
        self._is_first_request = True
        self._explanation_should_fail = False
        self._revision_should_fail = False

    async def request_command(
        self,
        request: str,
        on_progress: Callable[[int], None] | None = None,
    ) -> dict[str, Any]:
        """Generate a mock shell command from a natural language request.

        Recognizes scenario keywords to test different UX flows:
        - "error": Raises LLMGenerationError
        - "low confidence": Returns command with low confidence score
        - "explanation error": Sets flag for next explain_command() to fail
        - "revision error": Sets flag for next revision to fail
        - Any other text: Returns default successful response

        Parameters
        ----------
        request : str
            Natural language request or scenario keyword.
        on_progress : callable, optional
            Callback invoked with byte count as response chunks arrive.

        Returns
        -------
        dict[str, Any]
            Mock response dictionary containing:
            - command: Generated shell command
            - confidence: Confidence score (0.0-1.0)

        Raises
        ------
        LLMGenerationError
            If request is "error" or revision_should_fail flag is set.

        """
        # Simulate streaming with progress updates
        if on_progress:
            # Simulate receiving data in chunks
            for i in range(1, 6):
                await asyncio.sleep(0.3)
                on_progress(i * 50)  # Simulate 50, 100, 150, 200, 250 bytes
        else:
            await asyncio.sleep(1.5)

        # Handle revisions
        if not self._is_first_request:
            # Check revision error flag
            if self._revision_should_fail:
                self._revision_should_fail = False
                msg = "Mock revision error"
                raise LLMGenerationError(msg)

            # Return canned response for all revisions
            return {"command": "ls -la", "confidence": 0.85}

        # Mark that first request has been processed
        self._is_first_request = False

        # Parse scenario from request
        request_lower = request.lower().strip()

        if request_lower == "error":
            msg = "Mock generation error"
            raise LLMGenerationError(msg)
        if request_lower == "low confidence":
            return {"command": "find . -name '*.py'", "confidence": 0.3}
        if request_lower == "explanation error":
            self._explanation_should_fail = True
            return {"command": "grep -r 'pattern' .", "confidence": 0.85}
        if request_lower == "revision error":
            self._revision_should_fail = True
            return {"command": "git status", "confidence": 0.85}
        # Default scenario - real response from log
        return {
            "command": "du -ah ~/Downloads | sort -rh | head -n 5",
            "confidence": 0.95,
        }

    async def explain_command(
        self,
        on_progress: Callable[[int], None] | None = None,
    ) -> dict[str, Any]:
        """Explain the last generated command (mock implementation).

        Parameters
        ----------
        on_progress : callable, optional
            Callback invoked with byte count as response chunks arrive.

        Returns
        -------
        dict[str, Any]
            Mock explanation dictionary containing:
            - breakdown: List of component descriptions

        Raises
        ------
        LLMExplanationError
            If explanation_should_fail flag is set.

        """
        # Simulate streaming with progress updates
        if on_progress:
            # Simulate receiving data in chunks
            for i in range(1, 5):
                await asyncio.sleep(0.25)
                on_progress(i * 100)  # Simulate 100, 200, 300, 400 bytes
        else:
            await asyncio.sleep(1.0)

        # Check explanation error flag
        if self._explanation_should_fail:
            self._explanation_should_fail = False
            msg = "Mock explanation error"
            raise LLMExplanationError(msg)

        # Return canned explanation - hierarchical structure
        return {
            "breakdown": [
                {
                    "command": "du",
                    "description": "Estimates file space usage.",
                    "parts": [
                        {
                            "component": "-a",
                            "description": "Shows sizes for all files, not just directories.",
                        },
                        {
                            "component": "-h",
                            "description": "Prints sizes in human-readable format (e.g., KB, MB).",
                        },
                        {
                            "component": "~/Downloads",
                            "description": "Specifies the target directory to analyze.",
                        },
                    ],
                },
                {
                    "command": "sort",
                    "description": "Sorts the input lines.",
                    "parts": [
                        {
                            "component": "-r",
                            "description": "Reverses the sort order (descending).",
                        },
                        {
                            "component": "-h",
                            "description": "Sorts human-readable numbers (e.g., 2K, 1G).",
                        },
                    ],
                },
                {
                    "command": "head",
                    "description": "Outputs the first part of the input.",
                    "parts": [
                        {
                            "component": "-n 5",
                            "description": "Limits output to the first 5 lines.",
                        },
                    ],
                },
            ],
        }

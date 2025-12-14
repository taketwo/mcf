"""LLM Client for interacting with language models.

This module provides a simple interface to the llm library for
command generation using any accessible language model.
"""

from dataclasses import replace
from typing import TYPE_CHECKING, Any

from .logging import get_logger
from .utilities import (
    get_system_info,
    load_prompt_template,
    parse_json_response,
    render_prompt,
)

logger = get_logger(__name__)

if TYPE_CHECKING:
    from llm import AsyncConversation, AsyncModel


# JSON schemas for structured output
COMMAND_GENERATION_SCHEMA = {
    "type": "object",
    "properties": {
        "command": {"type": "string", "description": "The generated shell command"},
        "confidence": {
            "type": "number",
            "description": "Confidence score between 0.0 and 1.0",
            "minimum": 0.0,
            "maximum": 1.0,
        },
    },
    "required": ["command", "confidence"],
    "additionalProperties": False,
}

COMMAND_EXPLANATION_SCHEMA = {
    "type": "object",
    "properties": {
        "breakdown": {
            "type": "array",
            "description": "Hierarchical breakdown of the command by command stages",
            "items": {
                "type": "object",
                "properties": {
                    "command": {
                        "type": "string",
                        "description": "The base command name",
                    },
                    "description": {
                        "type": "string",
                        "description": "What this command does",
                    },
                    "parts": {
                        "type": "array",
                        "description": "Flags and arguments for this command",
                        "items": {
                            "type": "object",
                            "properties": {
                                "component": {
                                    "type": "string",
                                    "description": "Flag or argument",
                                },
                                "description": {
                                    "type": "string",
                                    "description": "What this part does",
                                },
                            },
                            "required": ["component", "description"],
                            "additionalProperties": False,
                        },
                    },
                },
                "required": ["command", "description", "parts"],
                "additionalProperties": False,
            },
        },
    },
    "required": ["breakdown"],
    "additionalProperties": False,
}


class LLMGenerationError(Exception):
    """Raised when LLM generation fails."""


class LLMExplanationError(Exception):
    """Raised when LLM explanation fails."""


class NoCommandGeneratedError(ValueError):
    """Raised when explanation is requested before any command is generated."""


class LLMClient:
    """Client for interacting with language models.

    This class provides methods to generate commands using language
    models through the llm library. It uses conversations to maintain
    context across multiple prompts, enabling proper conversation
    continuation for explanations.
    """

    def __init__(self, model_name: str = "llama3") -> None:
        """Initialize the LLM client.

        Parameters
        ----------
        model_name : str, optional
            Name of the model to use (default: "llama3").
            Can be any model accessible via the llm library
            (e.g., "llama3", "openrouter/model-name").

        """
        self.model_name = model_name
        self.system_info = get_system_info()
        self._conversation: AsyncConversation | None = None
        self._model: AsyncModel | None = None
        logger.info("Initialized LLM client with model: %s", model_name)

    async def _ensure_conversation(self) -> None:
        """Ensure a conversation object exists for this client.

        Creates a new conversation if one doesn't exist, or if
        the model has changed. This allows proper conversation
        continuation across multiple prompts.

        Raises
        ------
        LLMGenerationError
            If the llm library is not installed.

        """
        try:
            import llm  # noqa: PLC0415

            # Get the async model (create new if needed)
            if self._model is None:
                self._model = llm.get_async_model(self.model_name)

            # Create conversation if it doesn't exist
            if self._conversation is None:
                self._conversation = self._model.conversation()

        except ImportError as e:
            msg = "llm library not installed. Please install with: pip install llm"
            raise LLMGenerationError(msg) from e

    async def request_command(self, request: str) -> dict[str, Any]:
        """Generate or revise a shell command from a natural language request.

        This method automatically determines whether to start a new conversation
        (first request) or continue an existing one (revision).

        Parameters
        ----------
        request : str
            Natural language request for command generation or revision.

        Returns
        -------
        dict[str, Any]
            Parsed JSON response containing:
            - command: Generated shell command
            - confidence: Confidence score (0.0-1.0)

        Raises
        ------
        LLMGenerationError
            If the LLM response cannot be parsed or generation fails.

        """
        # Ensure conversation exists
        await self._ensure_conversation()
        if self._conversation is None:
            msg = "Failed to initialize conversation"
            raise LLMGenerationError(msg)

        # Determine which template to use based on conversation state
        is_first_request = len(self._conversation.responses) == 0
        if is_first_request:
            logger.info("Generating initial command")
            # First request - use generate template with system info
            template_content = load_prompt_template("generate_command")
            prompt = render_prompt(
                template_content,
                request=request,
                distro=self.system_info["distro"],
                shell=self.system_info["shell"],
                cwd=self.system_info["cwd"],
            )
        else:
            logger.info("Revising command based on user feedback")
            # Revision - use revise template
            template_content = load_prompt_template("revise_command")
            prompt = render_prompt(template_content, request=request)

        logger.debug("Prompt: %s", prompt)

        # Use conversation.prompt() with schema to enforce JSON output
        try:
            response = await self._conversation.prompt(
                prompt,
                schema=COMMAND_GENERATION_SCHEMA,
            )
            response_text = await response.text()
            logger.debug("LLM response: %s", response_text)

            # Parse and return the JSON response
            result = parse_json_response(response_text)
        except Exception as e:
            logger.exception("LLM generation failed")
            msg = f"LLM generation failed: {e}"
            raise LLMGenerationError(msg) from e
        else:
            logger.info("Successfully generated command")
            return result

    async def explain_command(self) -> dict[str, Any]:
        """Explain the last generated command by branching the conversation.

        This method creates an ephemeral branch of the current conversation,
        gets an explanation, then discards the branch so that explanations
        don't pollute the main conversation history for revisions.

        Returns
        -------
        dict[str, Any]
            Parsed JSON response containing:
            - breakdown: List of component descriptions

        Raises
        ------
        NoCommandGeneratedError
            If no command has been generated yet.
        LLMExplanationError
            If the LLM explanation fails.

        """
        logger.info("Requesting explanation for last command")

        # Ensure conversation exists
        await self._ensure_conversation()
        if self._conversation is None:
            msg = "Failed to initialize conversation"
            raise LLMExplanationError(msg)

        if len(self._conversation.responses) == 0:
            logger.warning("Attempted to explain command before generating one")
            msg = "No command has been generated yet. Generate a command first."
            raise NoCommandGeneratedError(msg)

        # Create an ephemeral branch of the conversation
        from llm.utils import monotonic_ulid  # noqa: PLC0415

        branch = replace(
            self._conversation,
            responses=self._conversation.responses.copy(),
            id=str(monotonic_ulid()).lower(),
        )
        logger.debug("Created ephemeral conversation branch for explanation")

        # Load and render the explanation prompt template
        template_content = load_prompt_template("explain_command")
        prompt = render_prompt(template_content)
        logger.debug("Explanation prompt: %s", prompt)

        # Get explanation on the branch
        try:
            response = await branch.prompt(prompt, schema=COMMAND_EXPLANATION_SCHEMA)
            response_text = await response.text()
            logger.debug("Explanation response: %s", response_text)

            # Parse and return the JSON response
            # Branch is discarded here (not assigned to self._conversation)
            result = parse_json_response(response_text)
        except Exception as e:
            logger.exception("LLM explanation failed")
            msg = f"LLM explanation failed: {e}"
            raise LLMExplanationError(msg) from e
        else:
            logger.info("Successfully generated explanation")
            return result

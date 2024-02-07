# Flush commands to bash history immediately.
# We should do this after starship has configured the prompt.
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

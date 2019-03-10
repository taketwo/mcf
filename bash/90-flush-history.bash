# Flush commands to bash history immediately.
# We do this after powerline has configured the prompt.
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

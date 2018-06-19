# Bash completion for tmuxp
if hash tmuxp 2>/dev/null; then
  eval "$(_TMUXP_COMPLETE=source tmuxp)"
fi

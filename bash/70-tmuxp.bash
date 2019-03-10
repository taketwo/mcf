# Bash completion for tmuxp
# The code below was produced by `_TMUXP_COMPLETE=source tmuxp`

_tmuxp_completion()
{
  COMPREPLY=( $( env COMP_WORDS="${COMP_WORDS[*]}" \
                 COMP_CWORD=$COMP_CWORD \
                 _TMUXP_COMPLETE=complete $1 ) )
  return 0
}

if hash tmuxp 2>/dev/null; then
  complete -F _tmuxp_completion -o default tmuxp;
fi

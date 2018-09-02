# Bash-independent part of the setup was performed in .profile
# The code below was produced by `pipenv --completion`

_pipenv_completion()
{
  local IFS=$'\t'
  COMPREPLY=( $( env COMP_WORDS="${COMP_WORDS[*]}" \
                 COMP_CWORD=$COMP_CWORD \
                 _PIPENV_COMPLETE=complete-bash $1 ) )
  return 0
}

if command -v pipenv 1>/dev/null 2>&1; then
  complete -F _pipenv_completion -o default pipenv
fi

function _update_ps1()
{
  export PS1="$(powerline-shell $?)"
}

if hash powerline-shell 2>/dev/null; then
  export PROMPT_COMMAND="_update_ps1"
fi

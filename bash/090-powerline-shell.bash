# Setup fancy "powerline-style" prompt
function _update_ps1()
{
  export PS1="$(~/.mcf/scripts/bundle/powerline-shell/powerline-shell.py $?) "
}
export PROMPT_COMMAND="_update_ps1"

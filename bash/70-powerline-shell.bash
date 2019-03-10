function _update_ps1()
{
  # Replace the last character output by powerline-shell (which is a regular space) with a non-breaking space
  # This is to enable the Tmux prompt jump trick: https://www.youtube.com/watch?v=uglorjY0Ntg
  local powerline_output
  powerline_output="$(powerline-shell $?)"
  export PS1="${powerline_output% } "
}

if hash powerline-shell 2>/dev/null; then
  export PROMPT_COMMAND="_update_ps1"
fi

# Set ag as the default source for fzf
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'

# Default options
export FZF_DEFAULT_OPTS='--ansi --select-1 --bind=alt-c:up,alt-t:down,alt-h:backward-char,alt-n:forward-char,tab:toggle-sort'

# Setup fzf if exists
[ -f ~/.fzf.bash ] && source ~/.fzf.bash

# Claim back Alt-c
bind '"\ec": history-search-backward'

# Integration with z
# - like normal z when used with arguments
# - displays an fzf prompt when used without
unalias z 2> /dev/null
z()
{
  if [[ -z "$*" ]]; then
    cd "$(_z -l 2>&1 | fzf +s --tac | sed 's/^[0-9,.]* *//')"
  else
    _z "$@"
  fi
}

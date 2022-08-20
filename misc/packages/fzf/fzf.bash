if command -v fzf-share >/dev/null; then
  # We do not want any bindings, however we want Bash functions defined in key-bindings.bash
  alias bind=':'  # Make bind a no-op
  source "$(fzf-share)/key-bindings.bash"
  unalias bind    # Restore bind
  source "$(fzf-share)/completion.bash"
fi

base03="234"
base02="235"
base01="240"
base00="241"
base0="244"
base1="245"
base2="254"
base3="230"
yellow="136"
orange="166"
red="160"
magenta="125"
violet="61"
blue="33"
cyan="37"
green="64"

# Colors for Solarized (light) palette
export FZF_COLORS="--color fg:-1,bg:-1,hl:$blue,fg+:$base02,bg+:$base2,hl+:$blue --color info:$yellow,prompt:$yellow,pointer:$base03,marker:$base03,spinner:$yellow"

# Set ripgrep as the default source for fzf
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'

# Default options
export FZF_DEFAULT_OPTS='--ansi --select-1 --bind=alt-c:up,alt-t:down,alt-h:backward-char,alt-n:forward-char,alt-s:toggle-sort '$FZF_COLORS

# CTRL-R - Paste the selected command from history into the command line
bind -x '"\C-r": __fzf_history__'

#######################################################################
#                            From FZF wiki                            #
#######################################################################

# fkill - kill process
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')
  if [ "x$pid" != "x" ]; then
    echo "$pid" | xargs kill -${1:-9}
  fi
}

# fv - fuzzy vim (with pygmentized preview)
fv() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0 --preview 'pygmentize -g -f terminal256 -P style=solarizedlight {} | head -100'))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# fpkg - fuzzy dpkg (with package status preview)
fpkg() {
  local packages
  packages=$(dpkg -l | awk '{print $2}') &&
    package=$(echo "$packages" | fzf --preview 'dpkg -s {}')
}


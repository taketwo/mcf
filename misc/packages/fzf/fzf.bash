# Some (most) of the color codes are not used in the theme, but are kept here for reference.
# The directive below disables the shellcheck warning for unused variables.
# shellcheck disable=2034

if command -v fzf-share >/dev/null; then
  # We do not want any bindings, however we want Bash functions defined in key-bindings.bash
  alias bind=':' # Make bind a no-op
  source "$(fzf-share)/key-bindings.bash"
  unalias bind # Restore bind
  source "$(fzf-share)/completion.bash"
fi

black="#101012"
bg0="#232326"
bg1="#2c2d31"
bg2="#35363b"
bg3="#37383d"
bg_d="#1b1c1e"
bg_blue="#68aee8"
bg_yellow="#e2c792"
fg="#a7aab0"
purple="#bb70d2"
green="#8fb573"
orange="#c49060"
blue="#57a5e5"
yellow="#dbb671"
cyan="#51a8b3"
red="#de5d68"
grey="#5a5b5e"
light_grey="#818387"
dark_cyan="#2b5d63"
dark_red="#833b3b"
dark_yellow="#7c5c20"
dark_purple="#79428a"

# Colors for One Dark (Warmer) palette
#   fg           Text
#   bg           Background
#   preview-fg   Preview window text
#   preview-bg   Preview window background
#   hl           Highlighted substrings
#   fg+          Text (current line)
#   bg+          Background (current line)
#   gutter       Gutter on the left (defaults to bg+)
#   hl+          Highlighted substrings (current line)
#   info         Info
#   border       Border of the preview window and horizontal separators (--border)
#   prompt       Prompt
#   pointer      Pointer to the current line
#   marker       Multi-select marker
#   spinner      Streaming input indicator
#   header       Header
export FZF_COLORS="
  --color fg:-1
  --color bg:-1
  --color hl:$blue
  --color fg+:-1
  --color bg+:$bg3
  --color gutter:$bg0
  --color hl+:$blue
  --color info:$yellow
  --color border:$bg3
  --color prompt:$blue
  --color pointer:$blue
  --color marker:$dark_red
  --color spinner:$yellow
"

export FZF_INDICATORS="
  --marker '⚫'
"

# Set ripgrep as the default source for fzf
export FZF_DEFAULT_COMMAND='rg --files --hidden --follow -g "!{.git,node_modules}/*" 2> /dev/null'

# Default options
export FZF_DEFAULT_OPTS='--ansi --select-1 --bind=alt-c:up,alt-t:down,alt-h:backward-char,alt-n:forward-char,alt-s:toggle-sort '$FZF_COLORS' '$FZF_INDICATORS

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
    echo "$pid" | xargs kill -"${1:-9}"
  fi
}

# fv - fuzzy vim (with pygmentized preview)
fv() {
  local files
  IFS=$'\n' files=($(fzf-tmux --query="$1" --multi --select-1 --exit-0 --preview 'pygmentize -g -f terminal256 -P style=one-dark {} | head -100'))
  [[ -n "$files" ]] && ${EDITOR:-vim} "${files[@]}"
}

# fpkg - fuzzy dpkg (with package status preview)
fpkg() {
  local packages
  packages=$(dpkg -l | awk '{print $2}') &&
    package=$(echo "$packages" | fzf --preview 'dpkg -s {}')
}

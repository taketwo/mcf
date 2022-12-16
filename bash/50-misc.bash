alias pt='sudo powertop'
alias x='exit'
alias chmox='chmod +x'

# Create a directory and cd into it
function mkd() {
  mkdir -p "$@" && eval cd "\"\$$#\""
}

# Repeat last command with "c"
# Useful because sometimes "c" is produced on Alt-c press
alias c='eval `fc -ln -2 -2 | sed -e "s/^[[:space:]]*//"`'

# Out-of-source build with CMake
alias osb='mkd build && cmake ..'

# Copy and paste to clipboard
alias cbc='xsel --clipboard --input'
alias cbp='xsel --clipboard --output'

# Git
alias ga='gitk --all'
alias gg='git gui'

# Latex
alias tex-clean='rm -f *.{aux,log,out,bbl,blg}'

if ! hash see 2>/dev/null; then
  alias see='xdg-open'
fi

# Use GitHub theme in bat
export BAT_THEME="GitHub"

# Open pointcloud in PCL viewer with axes displayed
alias pax='pcl_viewer -ax 0.2'

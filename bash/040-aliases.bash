alias pt='sudo powertop'
alias x='exit'
alias chmox='chmod +x'
alias e='vim'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# quick out-of-source build preparation
alias osb='mkd build && ccmake ..'

# quick background color switch
alias dark='~/.mcf/misc/gnome-terminal-colors-solarized/set_dark.sh'
alias light='~/.mcf/misc/gnome-terminal-colors-solarized/set_light.sh'

# copy and paste to clipboard
alias cbc='xsel --clipboard --input'
alias cbp='xsel --clipboard --output'

# misc
alias ga='gitk --all'
alias gg='git gui'
alias ff='find . -name $*'
alias vundle='vim +BundleInstall +qall'
alias tex-clean='rm -f *.{aux,log,out,bbl,blg}'
alias trayer-show='trayer --align center --edge top --SetDockType false --SetPartialStrut false --widthtype request --transparent false --tint 0xFFFFFF --heighttype request --distancefrom top --distance 400 --monitor 1 --padding 20'

if ! hash see 2>/dev/null; then
  alias see='xdg-open'
fi


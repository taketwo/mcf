# Enable color support of ls and grep
if [ -x /usr/bin/dircolors ]; then
  # Install dircolors for Solarized theme
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors/dircolors.256dark)"
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

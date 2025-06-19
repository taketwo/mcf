# Install dircolors
test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors/dircolors.one-dark)"
alias ls='ls --color=auto'
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

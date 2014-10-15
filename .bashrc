# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
# ... or force ignoredups and ignorespace
HISTCONTROL=ignoredups:ignorespace

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=25000
HISTFILESIZE=50000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

export TERM='xterm-256color'

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    # install dircolors for Solarized theme
    test -r ~/.dircolors && eval `dircolors ~/.dircolors/dircolors.256dark`
    # test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    # alias dir='dir --color=auto'
    # alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# disable XON/XOFF so that Ctrl-S combination works in reverse-i-search
stty -ixon

# Make sure that $MCF is set
if [[ -z "$MCF" ]]; then
  export MCF=/home/sergey/.mcf
fi

export EDITOR=vim
export BROWSER=chromium-browser

load ()
{
  script=$MCF/bash/$1.bash
  if [ -f $script ] ; then
    . $script
  fi
}

load archives
load navigation
load touchpad
load network
load man
load stow

# quick out-of-source build preparation
alias osb='mkd build && ccmake ..'

# apt-get aliases
alias agu='sudo apt-get update && sudo apt-get upgrade'
alias agi='sudo apt-get install'
alias agr='sudo apt-get remove'

# yaourt aliases
alias yu='yaourt -Syua'
alias yi='yaourt -Sa'
alias yr='yaourt -Rc'

# quick background color switch
alias dark='~/.mcf/misc/gnome-terminal-colors-solarized/set_dark.sh'
alias light='~/.mcf/misc/gnome-terminal-colors-solarized/set_light.sh'

# copy and paste to clipboard
alias cbc='xsel --clipboard --input'
alias cbp='xsel --clipboard --output'

# misc
alias pt='sudo powertop'
alias ga='gitk --all'
alias gg='git gui'
alias x='exit'
alias ff='find . -name $*'
alias vundle='vim +BundleInstall +qall'
alias rs='nohup redshift -l 50.4:7.5 -t 6000:4500 &'
alias tex-clean='rm -f *.{aux,log,out,bbl,blg}'
alias trayer-show='trayer --align center --edge top --SetDockType false --SetPartialStrut false --widthtype request --transparent false --tint 0xFFFFFF --heighttype request --distancefrom top --distance 400 --monitor 1 --padding 20'
alias msync='rsync -avz --size-only --progress --delete /media/Files/Music/ ex:sdcard1/Music'
alias chmox='chmod +x'

if ! hash see 2>/dev/null; then
  alias see='xdg-open'
fi

less ()
{
  pygmentize -g -f terminal256 -P style=solarizedlight "$@" | /usr/bin/less -FiXRM
}

# relaunch the last command in gdb
gdb-last-command ()
{
  cmd=`fc -ln -2 -2 | sed -e 's/^[[:space:]]*//'`
  bin=`cut -d ' ' -f 1 <<< $cmd`
  arg=`cut -d ' ' -f 2- <<< $cmd`
  gdb $bin -ex "run $arg"
}

# set terminal window title
title ()
{
  echo -ne "\033]0;$1\007"
}

encrypt ()
{
  openssl aes-256-cbc -a -salt -in $1 -out $1.enc
}

decrypt ()
{
  openssl aes-256-cbc -a -d -in $1 -out $1.dec
}

DISTRO=`cat /etc/*-release | grep -Po '(?<=DISTRIB_ID=)(.*)'`
if [[ $DISTRO == "Arch" ]]; then
  alias file-from-package='pkgfile'
  alias files-in-package='pacman -Ql'
else
  alias file-from-package='apt-file search'
  alias files-in-package='dpkg-query -L'
fi

vim-ag()
{
  vim -s <(printf "\e;Ag $1\n")
}
alias e='vim'

export PATH=$PATH:$MCF/scripts/bin

# Source local configuration if it exists
if [ -f ~/.bashrc_local ]; then
    . ~/.bashrc_local
fi

# Load secret configuration settings
if [ -f ~/.secrets ]; then
    . ~/.secrets
fi

# Setup fancy "powerline-style" prompt
function _update_ps1()
{
  export PS1="$(~/.mcf/scripts/bundle/powerline-shell/powerline-shell.py $?) "
}
export PROMPT_COMMAND="_update_ps1"

# Setup z for quick jumping around
source $MCF/scripts/bundle/z/z.sh

# remove load function
unset -f load

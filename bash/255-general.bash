#!/usr/bin/env bash

# Filename:      050-misc-config
# Description:   Misc configuration.

# Stolen from "https://github.com/jdevera/dotfiles"

# Section: Files and directories (ENV) {{{
#############################################################################

# Directories
if [[ -z "$MCF" ]]; then
  export MCF=/home/sergey/.mcf
fi

#############################################################################

# }}}
# Section: PATH {{{
#############################################################################

pathprepend "$MCF/scripts/bin"
pathprepend "$HOME/.local/bin"

#############################################################################

# }}}
# Section: Apps {{{
#############################################################################

export EDITOR=vim
export BROWSER=browser
export PAGER=less

# Enable color support of ls and grep
if [ -x /usr/bin/dircolors ]; then
    # install dircolors for Solarized theme
    test -r ~/.dircolors && eval `dircolors ~/.dircolors/dircolors.256dark`
    alias ls='ls --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

#############################################################################

# }}}

# Section: History {{{
#############################################################################

# HISTCONTROL {{{
# ===========
# A colon-separated list of values controlling how commands are saved on the
# history list. If the list of values includes ignorespace, lines which begin
# with a space character are not saved in the history list. A value of
# ignoredups causes lines matching the previous history entry to not be saved.
# A value of ignoreboth is shorthand for ignorespace and ignoredups. A value of
# erasedups causes all previous lines matching the current line to be removed
# from the history list before that line is saved. Any value not in the above
# list is ignored. If HISTCONTROL is unset, or does not include a valid value,
# all lines read by the shell parser are saved on the history list, subject to
# the value of HISTIGNORE. The second and subsequent lines of a multi-line
# compound command are not tested, and are added to the history regardless of
# the value of HISTCONTROL.
HISTCONTROL=ignoredups:ignorespace

# }}}
# HISTSIZE {{{
# ========
# The number of commands to remember in the command history. The default value
# is 500.
HISTSIZE=25000

# }}}
# HISTFILESIZE {{{
# ============
# The maximum number of lines contained in the history file. When this variable
# is assigned a value, the history file is truncated, if necessary, by removing
# the oldest entries, to contain no more than that number of  lines. The
# default value is 500. The history file is also truncated to this size after
# writing it when an interactive shell exits.
HISTFILESIZE=100000

# }}}
# HISTIGNORE {{{
# ==========
# A colon-separated list of patterns used to decide which command lines should
# be saved on the history list. Each pattern is anchored at the beginning of
# the line and must match the complete line (no implicit `*' is appended). Each
# pattern is tested against the line after the checks specified by HISTCONTROL
# are applied. In addition to the normal shell pattern matching characters, `&'
# matches the previous history line. `&' may be escaped using a backslash; the
# backslash is removed before attempting a match. The second and subsequent
# lines of a multi-line compound command are not tested, and are added to the
# history regardless of the value of HISTIGNORE.
HISTIGNORE="?:??:cd *:keyring-password *:pwd:cdd *:p"

# }}}
# HISTTIMEFORMAT {{{
# ==============
# If this variable is set and not null, its value is used as a format string
# for strftime(3) to print the time stamp associated with each history entry
# displayed by the history builtin.  If this variable is set, time stamps are
# written to the history file so they may be preserved across shell sessions.
# This uses the history comment character to distinguish timestamps from other
# history lines.
HISTTIMEFORMAT="%Y-%m-%d %T %z "

# }}}
# shopt: histappend {{{
# =================
# If set, the history list is appended to the file named by the value of the
# HISTFILE variable when the shell exits, rather than overwriting the file.
shopt -s histappend

# }}}
#############################################################################

# }}}
# Section: Bash Options {{{
#############################################################################
# checkwinsize {{{
# ------------
#
# Check the window size after each command and, if necessary, update the
# values of LINES and COLUMNS.
shopt -s checkwinsize

# }}}
# cdspell {{{
# -------
#
# If set, minor errors in the spelling of a directory component in a cd command
# will be corrected. The errors checked for are transposed characters, a
# missing character, and a character too many. If a correction is found, the
# corrected path is printed, and the command proceeds. This option is only used
# by interactive shells.
shopt -s cdspell

# }}}
# extglob {{{
# -------
#
# If set, several extended pattern matching operators are recognized:
#   ?(pattern-list) Matches zero or one occurrence of the given patterns
#   *(pattern-list) Matches zero or more occurrences of the given patterns
#   +(pattern-list) Matches one or more occurrences of the given patterns
#   @(pattern-list) Matches one of the given patterns
#   !(pattern-list) Matches anything except one of the given patterns
shopt -s extglob

# }}}
#############################################################################

# }}}

# disable XON/XOFF so that Ctrl-S combination works in reverse-i-search
stty -ixon

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

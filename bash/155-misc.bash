#!/usr/bin/env bash

# Filename:      010-misc-functions.bash
# Description:   Misc functions.

# Stolen from "https://github.com/jdevera/dotfiles"

function highlight()
{
   pygmentize -g -f terminal256 -P style=solarizedlight "$@"
}

hl()
{
   highlight "$@" | /usr/bin/less -FiXRM
}

#______________________________________________________________________________
# Check whether the argument is a runnable command: shell built-in, alias,
# function, or file in the PATH
#______________________________________________________________________________
#
function has_command()
{
   type "$1" >& /dev/null
}
#______________________________________________________________________________


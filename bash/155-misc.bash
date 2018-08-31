#!/usr/bin/env bash

# Filename:      010-misc-functions.bash
# Description:   Misc functions.

# Stolen from "https://github.com/jdevera/dotfiles"

function bashtimes()
{
   [[ -f /tmp/bashtimes.$$ ]] || return 1
   awk '
   {
      if (NR==1) {
         first=$2
      }
      last=$3
      ms = ($3 - $2) / 1000000
      printf( "%6.2f ms %s\n", ms, $1)
   }
   END {
      printf("Total: %6.2f ms\n",  (last-first) / 1000000 )
   }
   ' /tmp/bashtimes.$$ |

   sort -k1 -n -r
}

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


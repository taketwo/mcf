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

function hl()
{
   pygmentize -g -f terminal256 -P style=solarizedlight "$@"
}

hless()
{
   hl "$@" | /usr/bin/less -FiXRM
}

#______________________________________________________________________________
#
# Find the type of executable "thing" that the shell will use and try to
# describe it in its output:
#
# For an alias, print its definition
# For a function, print its code
# For a shell builtin, print its help text
# For a script, print the source
# For a binary executable file, print nothing.
#______________________________________________________________________________
#
function code()
{
   local type="$(builtin type -t $1)"
   case $type in
      alias)
         echo "$1 is an alias"
         builtin alias "$1" | sed 's/^[^=]\+=//'
         ;;
      function)
         echo "$1 is a function"
         find_function $1 | awk '{ printf("Defined in: %s +%d\n", $3, $2) }'
         builtin declare -f "$1" | hless
         ;;
      builtin | keyword)
         echo "$1 is a shell $type"
         builtin help "$1"
         ;;
      file)
         local path="$(which "$1")"
         if head -1 "$path" | grep -q "^#!"; then
            echo "$1 is a script at $path"
            cat "$path" | hless
         else
            echo "$1 is a binary at $path"
         fi
         ;;
      *)
         echo "I don't know what $1 is"
         return 1
         ;;
   esac
}
complete -c code # Complete with command names
#______________________________________________________________________________


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


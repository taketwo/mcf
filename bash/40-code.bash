#!/usr/bin/env bash

# Based on: https://github.com/jdevera/dotfiles

# Find the type of executable "thing" that the shell will use and try to
# describe it in its output:
#   - for an alias, print its definition
#   - for a function, print its code
#   - for a shell builtin, print its help text
#   - for a script, print the source
#   - for a binary executable file, print nothing.
function code()
{
  local type="$(builtin type -t "$1")"
  case $type in
    alias)
      echo "$1 is an alias"
      builtin alias "$1" | sed 's/^[^=]\+=//'
      ;;
    function)
      echo "$1 is a function"
      find_function "$1" | awk '{ printf("Defined in: %s +%d\n", $3, $2) }'
      builtin declare -f "$1" | bat -l bash --style plain
      ;;
    builtin | keyword)
      echo "$1 is a shell $type"
      builtin help "$1" | bat --style plain
      ;;
    file)
      local path="$(command -v "$1")"
      if head -1 "$path" | grep -q "^#!"; then
        echo "$1 is a script at $path"
        bat "$path"
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


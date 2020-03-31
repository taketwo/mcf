#!/usr/bin/env bash

# Utility functions for handling bash functions
# Based on: https://github.com/jdevera/dotfiles

# Check if a function given by name is defined in the current shell instance
function function_exists() {
  local a_function_name="$1"

  [[ -z $a_function_name ]] && return 1

  declare -F "$a_function_name" >/dev/null 2>&1

  return $?
}

# If the function specified in the first parameter exists, call it with the
# subsequent parameters
function call_if() {
  function_exists "$1" && "$@"
}

# Find the location where a function has been defined
function find_function() {
  (
    shopt -s extdebug
    declare -F "$1"
  )
}

# Show a pretty printed list of shell functions, sorted by file and line
function list_functions() {
  # Get the functions with the origin files and lines
  (
    shopt -s extdebug
    for f in $(declare -F | awk '{ print $3 }'); do
      declare -F "$f"
    done
  ) |

    # Filter out bash_completion functions, there are too many
    grep -v bash_completion |

    # Sort by file first and then by line
    sort -k3 -k2.1n |

    # Format grouped by file
    awk 'BEGIN{
      prev = 0
   }
   {
      if (prev != $3) {
         print "\n" $3
         prev = $3
      }
      printf("%4s\t%s\n", $2, $1)
   }'
}

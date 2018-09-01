#!/usr/bin/env bash

# Utility functions for handling strings
# Based on: https://github.com/jdevera/dotfiles

# Split a string printing each slice on its own line
# Parameters:   1 (o): Delimiter string
#               2 (o): Input file name; will use standard input if none is provided
function string_split()
{
  local DEFAULT_DELIMITER=" "

  local a_delimiter="${1:-$DEFAULT_DELIMITER}"
  local a_inputfile="$2"

  awk -F "$a_delimiter" \
    '{  for(i = 1; i <= NF; i++) {
            print $i
        }
    }' "$a_inputfile"

  return $?
}

# Join several lines in one using a custom delimiter
# Parameters:   1 (o): Delimiter string
#               2 (o): Input file name; will use standard input if none is provided
function string_join()
{
  local DEFAULT_DELIMITER=" "

  local a_delimiter="${1:-$DEFAULT_DELIMITER}"
  local a_inputfile="$2"

  awk -v usersep="$a_delimiter" '
    BEGIN{
            sep=""; # Start with no separator (before the first item)
    }
    {
            printf("%s%s", sep, $0);
            (NR == 1) && sep = usersep; # Separator is set after the first item.
    }
    END{
            print "" # Print a new line at the end.
  }' "$a_inputfile"
  return $?
}

# Remove all appearances of a field from a delimited list, making sure that
# delimiters are consistent afterward
# Parameters:   1 (r): Element to be removed
#               2 (o): Delimiter string
#               3 (o): Input file name; will use standard input if none is provided.
function remove_from_list()
{
  local DEFAULT_DELIMITER=" "

  local a_element="$1"
  local a_delimiter="${2:-$DEFAULT_DELIMITER}"
  local a_inputfile="$3"

  [[ -z $a_element ]] && return 1

  string_split "$a_delimiter" "$a_inputfile" | \
    grep -v "^$a_element\$" | \
    string_join "$a_delimiter"

  return $?
}

# Trim leading and trailing white-space from a string
# Source: https://github.com/dylanaraps/pure-bash-bible#trim-leading-and-trailing-white-space-from-string
function string_trim()
{
  : "${1#"${1%%[![:space:]]*}"}"
  : "${_%"${_##*[![:space:]]}"}"
  printf '%s\n' "$_"
}


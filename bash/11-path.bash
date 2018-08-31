#!/usr/bin/env bash

# Utility functions for PATH manipulation
# Based on: https://github.com/jdevera/dotfiles

# Add a directory to the $PATH enviroment variable.
#   - Checks that the directory exists.
#   - Directories can be inserted at the beginning or the end of
#     the $PATH.
#   - If the directory is already in the $PATH, it will be moved
#     to the requested position.
# Parameters:   1 (r): Directory that is to be inserted in $PATH
#               2 (r): Position for insertion, accepted values are:
#                      - "beg" to insert at the beginning
#                      - "end" to insert at the end
function addtopath()
{
   local a_directory="$1"
   local a_position="$2"

   a_directory="${a_directory/%\//}"  # remove trailing slash

   # Add only existing directories
   [[ ! -d $a_directory ]] && return 1

   # If the directory is already in the path, remove it so that
   # it can be inserted in the desired position without
   # poluting $PATH with duplicates
   local newpath=$(echo "$PATH" | remove_from_list "$a_directory" ':')

   if [[ $a_position == beg ]]; then    # Prefix to $PATH
      export PATH="$a_directory:$newpath"
   elif [[ $a_position == end ]]; then  # Append to $PATH
      export PATH="$newpath:$a_directory"
   else
      return 1
   fi

   return 0
}

# Convenience wrappers for addtopath
function pathappend()  { addtopath "$1" end; return $?; }
function pathprepend() { addtopath "$1" beg; return $?; }

# Delete a directory from the $PATH enviroment variable.
# Parameters:   1 (r): Directory to delete
function delfrompath()
{
   local a_directory="$1"
   export PATH=$(echo "$PATH" | remove_from_list "$a_directory" ':')
}


#!/usr/bin/env bash

# Utility functions for handling completion functions
# Based on: https://github.com/jdevera/dotfiles

# Create a completion function for an alias by wrapping the completion function
# for the original command.
# Parameters:   1 (r): Actual completion function
#               2 (r): Name of the new generated function
#               3 (r): Name of the alias
#               4 (r): Original command name
#             5.. (r): List of arguments fixed by the alias
# Help:
#   http://ubuntuforums.org/showthread.php?t=733397
#   For example, to define a function called _apt_get_install that will
#   complete the 'agi' alias:
#      alias agi='apt-get install'
#      make_completion_wrapper _apt_get _apt_get_install agi apt-get install
#      complete -F _apt_get_install agi
function make_completion_wrapper() {
  local comp_function_name="$1"
  local function_name="$2"
  local alias_name="$3"
  local arg_count=$(($# - 4))
  shift 3
  local args="$*"
  local function="
function $function_name {
  COMP_LINE=\"$@\${COMP_LINE#$alias_name}\"
  let COMP_POINT+=$((${#args} - ${#alias_name}))
  ((COMP_CWORD+=$arg_count))
  COMP_WORDS=("$@" \"\${COMP_WORDS[@]:1}\")

  local cur words cword prev
  _get_comp_words_by_ref -n =: cur words cword prev
  "$comp_function_name"
  return 0
}"
  eval "$function"
}

# Assign a function called __complete_<blah> to a command named <blah>. The name
# of the command is a parameter.
# Parameters:   1 (r): The name of the command that is used to derive the
#                      completion function name
#               2 (o): Optionally, bind the derived name to this command. This
#                      is used to prevent having to duplicate the function if
#                      it is to be applied to several aliases of the same
#                      command.
# Help:
#   For example, to apply a custom completion function for a `foo` command
#   and its alias `bar`:
#
#      link_complete_function foo
#
#   will use __complete_foo to complete foo, and
#
#      link_complete_function foo bar
#
#   will use also __complete_foo to complete bar
function link_complete_function() {
  eval "complete -F __complete_$1 -o default ${2:-$1}"
}

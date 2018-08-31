#!/usr/bin/env bash

# Utility functions
# Based on: https://github.com/jdevera/dotfiles

#______________________________________________________________________________
#
# FUNCTION:     showenv
#
# DESCRIPTION:  Show a pretty printed list of environment variables.
#
# PARAMETERS:   None
#______________________________________________________________________________
#
function showenv()
{
   printenv | grep = | showaliases -a -
}
#______________________________________________________________________________


#______________________________________________________________________________
#
# FUNCTION:     reloadsh
#
# DESCRIPTION:  Reload all bash's config files after clearing all defined
#               functions.
#
# PARAMETERS:   None
#______________________________________________________________________________
#
function reloadsh()
{
   for f in $(declare -F |  awk '{ print $3 }')
   do
      unset -f $f
   done
   unalias -a
   KEEP_PROMPT=1 source $HOME/.bashrc
}
#______________________________________________________________________________


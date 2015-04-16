#!/bin/bash

# This implements an approach proposed by Magnus Thor Torfason
# (see http://permalink.gmane.org/gmane.comp.gnu.stow.general/6646)

STOW_DIR=/opt/stow
STOW_TARGET=/usr/local

mktouch ()
{
  if [ "$1" == "" ] || [ "$2" != "" ]; then
    echo "Incorrect number of arguments to mktouch (expected exactly one)."
    exit 1
  fi
  sudo mkdir -p "$(dirname "$1")"
  sudo touch "$1"
}

export -f mktouch

stow-adopt-as ()
{
  if [ "$1" == "" ] || [ "$2" != "" ]; then
    echo "Incorrect number of arguments to stow-adopt-as (expected exactly one).";
    return 1;
  fi;
  PACKAGE="$1";
  chkstow -t $STOW_TARGET -a | sed "s+^Unstowed file: $STOW_TARGET+$STOW_DIR/$PACKAGE+" | xargs -l -i bash -c 'mktouch "$@"' _ {};
  sudo stow --adopt -t $STOW_TARGET -d $STOW_DIR -vv $PACKAGE
}

stow-install ()
{
  if [ "$1" == "" ] || [ "$2" != "" ]; then
    echo "Incorrect number of arguments to stow-delete (expected exactly one).";
    return 1;
  fi;
  PACKAGE="$1";
  sudo stow -t $STOW_TARGET -d $STOW_DIR -vv $PACKAGE
}

stow-delete ()
{
  if [ "$1" == "" ] || [ "$2" != "" ]; then
    echo "Incorrect number of arguments to stow-delete (expected exactly one).";
    return 1;
  fi;
  PACKAGE="$1";
  sudo stow -t $STOW_TARGET -d $STOW_DIR -vv --delete $PACKAGE
}

stow-show-orphans ()
{
  chkstow -a -t $STOW_TARGET
}

function __complete_stow ()
{
  packages=`ls "$STOW_DIR"`
  local current=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=($(compgen -W "$packages" -- $current))
}

# Small hack to avoid calling this function if it is not defined.
# This is needed because we explicitly source this file in stow.py.
if [[ "$(builtin type -t link_complete_function)" == "function" ]]; then
  link_complete_function stow stow-adopt-as
  link_complete_function stow stow-install
  link_complete_function stow stow-delete
fi


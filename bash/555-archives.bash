#!/usr/bin/env bash

# Filename:      010-archives.bash
# Description:   Utility functions for dealing with archive files.
# Maintainer:    Sergey Alexandrov <alexandrov88@gmail.com>

# <doc:archives> {{{
#
# Provides functions for working with archive files.
#
# </doc:archives> }}}

extract () #{{{1
{
  # <doc:extract> {{{
  #
  # Extracts an archieve if it has one of the known types
  #
  # Usage: extract FILE
  #
  # </doc:extract> }}}

  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xvjf $1                               ;;
      *.tar.gz)    tar xvzf $1                               ;;
      *.bz2)       bunzip2 $1                                ;;
      *.rar)       unrar x $1                                ;;
      *.gz)        gunzip $1                                 ;;
      *.tar)       tar xvf $1                                ;;
      *.tbz2)      tar xvjf $1                               ;;
      *.tgz)       tar xvzf $1                               ;;
      *.zip)       unzip $1                                  ;;
      *.Z)         uncompress $1                             ;;
      *.7z)        7z x $1                                   ;;
      *.deb)       ar p $1 data.tar.gz | tar zx              ;;
      *)           echo "Do not know how to extract '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file!"
  fi
}

pack () #{{{1
{
  # <doc:pack> {{{
  #
  # Packs a file or a directory in an archive of requested type
  #
  # Usage: pack TYPE FILE
  #
  # Supported archive types:
  #
  #     tbz
  #     tgz
  #     tar
  #     bz2
  #     gz
  #     zip
  #     7z
  #
  # </doc:pack> }}}

  if [ $1 ] ; then
    case $1 in
      tbz)      tar cjvf $2.tar.bz2 $2   ;;
      tgz)      tar czvf $2.tar.gz  $2   ;;
      tar)      tar cpvf $2.tar  $2      ;;
      bz2)      bzip2 $2                 ;;
      gz)       gzip -c -9 -n $2 > $2.gz ;;
      zip)      zip -r $2.zip $2         ;;
      7z)       7z a $2.7z $2            ;;
      *)        echo "Do not know how to pack in '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file!"
  fi
}

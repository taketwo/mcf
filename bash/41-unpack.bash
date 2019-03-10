#!/usr/bin/env bash

# Extracts an archieve if it has one of the known types
function unpack()
{
  if [ -f "$1" ] ; then
    case $1 in
      *.tar.bz2)   tar xvjf "$1"                             ;;
      *.tar.gz)    tar xvzf "$1"                             ;;
      *.tar.xz)    tar xvJf "$1"                             ;;
      *.bz2)       bunzip2 "$1"                              ;;
      *.rar)       unrar x "$1"                              ;;
      *.gz)        gunzip "$1"                               ;;
      *.tar)       tar xvf "$1"                              ;;
      *.tbz2)      tar xvjf "$1"                             ;;
      *.tgz)       tar xvzf "$1"                             ;;
      *.zip)       unzip "$1"                                ;;
      *.Z)         uncompress "$1"                           ;;
      *.7z)        7z x "$1"                                 ;;
      *.deb)       ar p "$1" data.tar.gz | tar zx            ;;
      *)           echo "Do not know how to extract '$1'..." ;;
    esac
  else
    echo "'$1' is not a valid file!"
  fi
}


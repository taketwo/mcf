#!/bin/bash

solarizedGreen="#859900"
solarizedRed="#dc322f"
solarizedBlue="#268bd2"
solarizedCyan="#2aa198"
solarizedYellow="#b58900"
solarizedBase3="#fdf6e3"

Start ()
{
  eval "output=' '"
}

Fg ()
{
  if [ "$output" != " " ] ; then
    eval "output='^fg($1)$output^fg()'"
  fi
}

Bg ()
{
  if [ "$output" != " " ] ; then
    eval "output='^bg($1)$output^bg()'"
  fi
}

Icon ()
{
  eval "output='$output^i(/home/sergey/.xmonad/icons/${1}.xbm) '"
}

Add ()
{
  eval "output='$output$1 '"
}

Flush ()
{
  if [ "$output" != " " ] ; then
    echo -n "$output"
  fi
}

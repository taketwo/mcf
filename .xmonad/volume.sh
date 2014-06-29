#!/bin/bash

# Requires amixer
#   pacman -S alsa-utils

source $MCF/.xmonad/panel.bash

volume=`amixer get Master | awk -F'[]%[]' 'BEGIN { nlines = 0 } /%/ { if (nlines == 0) { if ($5 == "off") { print "0" } else { print $2 }; nlines++}}'`

Start

if (( $volume == 0 )); then
  Icon "mute"
  Fg $solarizedBlue
else
  Icon "volume"
  if (( volume < 100 )); then
    Add $volume
  fi
fi

Flush

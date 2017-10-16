#!/bin/bash

# Requires amixer
#   pacman -S alsa-utils

source $MCF/.xmonad/solarized.bash
source $MCF/.xmonad/xmobar.bash

volume=`amixer -D pulse sget Master | awk -F'[]%[]' 'BEGIN { nlines = 0 } /%/ { if (nlines == 0) { if ($5 == "off" || $7 == "off") { print "0" } else { print $2 }; nlines++}}'`

StartIndicator

if (( $volume == 0 )); then
  Symbol "mute"
  Color $SolarizedBlue
  Action 1 "amixer -D pulse set Master on"
else
  Symbol "volume"
  if (( volume < 100 )); then
    Add $volume
  fi
  Action 1 "amixer -D pulse set Master off"
fi

FlushIndicator

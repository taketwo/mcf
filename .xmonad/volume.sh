#!/bin/bash

# Requires amixer
#   pacman -S alsa-utils

volume=`amixer get Master | awk -F'[]%[]' 'BEGIN { nlines = 0 } /%/ { if (nlines == 0) { if ($5 == "off") { print "0" } else { print $2 }; nlines++}}'`

solarizedRed="#dc322f"
solarizedBase3="#fdf6e3"

mute="^i(/home/sergey/.xmonad/icons/mute.xbm)"
vol="^i(/home/sergey/.xmonad/icons/volume.xbm)"

if [[ $volume = 0 ]]; then
  echo -n " $mute "
else
  echo -n " $vol $volume "
fi

#!/bin/bash

volume=`amixer get Master | awk -F'[]%[]' '/%/ {if ($7 == "off") { print "0" } else { print $2 }}'`

solarizedRed="#dc322f"
solarizedBase3="#fdf6e3"

mute="^i(/home/sergey/.xmonad/icons/mute.xbm)"
vol="^i(/home/sergey/.xmonad/icons/volume.xbm)"

if [[ $volume = 0 ]]; then
  echo -n "^bg($solarizedRed)^fg($solarizedBase3) $mute ^fg()^bg()"
else
  echo -n " $vol $volume "
fi

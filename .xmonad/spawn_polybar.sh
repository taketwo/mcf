#!/usr/bin/env bash

if hash xrandr 2>/dev/null; then
  num_monitors=$(xrandr --query | grep -c " connected [0-9]")
  if [[ $num_monitors -eq "3" ]]; then
    export MONITOR="DP-1-2-2"
  else
    export MONITOR=$(polybar -m | tail -1 | sed -e 's/:.*$//g')
  fi
fi

polybar -c "$MCF/.xmonad/polybarrc" -r primary

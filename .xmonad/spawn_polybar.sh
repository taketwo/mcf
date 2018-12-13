#!/usr/bin/env bash

if [[ $HOSTNAME == "ramen" ]]; then
  export ETH_INTERFACE="enp4s0"
elif [[ $HOSTNAME == "raccoon" ]]; then
  export ETH_INTERFACE="enp0s25"
fi

# List all monitors sorted by horizontal position
mapfile -t monitors < <(polybar -m | awk -F'[: x+]' '{print $5 " " $1}' | sort | awk '{print $2}')
# Select the middle one
index=$(((${#monitors[@]} - 1) / 2))
export MONITOR=${monitors[$index]}

# Kill all gmail.py scripts
pkill -f gmail.py

polybar -c "$MCF/.xmonad/polybarrc" -r primary

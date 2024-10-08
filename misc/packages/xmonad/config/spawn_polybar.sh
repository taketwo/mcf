#!/usr/bin/env bash

if [[ $HOSTNAME == "ramen" ]]; then
  export ETH_INTERFACE="enp4s0"
elif [[ $HOSTNAME == "alpaca" ]]; then
  # Find using `pacmd list-sinks | grep name`
  export PULSEAUDIO_SINK="alsa_output.usb-C-Media_Electronics_Inc._Mpow_HC_20170817-00.iec958-stereo"
  export ETH_INTERFACE="enp4s0"
elif [[ $HOSTNAME == "raccoon" ]]; then
  export ETH_INTERFACE="enp0s25"
  export WLAN_INTERFACE="wlp3s0"
elif [[ $HOSTNAME == "lemur" ]]; then
  export ETH_INTERFACE="enxa0cec86c6f27"
  export WLAN_INTERFACE="wlp2s0"
fi

# List all monitors sorted by horizontal position
mapfile -t monitors < <(polybar -m | awk -F'[: x+]' '{print $5 " " $1}' | sort | awk '{print $2}')
# Select the middle one, round down
index=$(((${#monitors[@]} - 1) / 2))
export MONITOR=${monitors[$index]}

# Kill all gmail.py scripts
pkill -f gmail.py

polybar -c "$HOME/.xmonad/polybarrc" -r primary

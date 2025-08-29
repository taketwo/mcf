#!/usr/bin/env bash

log() {
  echo "$(date '+%Y-%m-%d %H:%M:%S'): $1" >>/tmp/polybar_spawn.log
}

# Trap signals to log termination
trap 'log "Received SIGTERM, polybar may be terminating"; exit 0' TERM
trap 'log "Received SIGINT, polybar may be terminating"; exit 0' INT
trap 'log "Received SIGHUP, polybar may be terminating"; exit 0' HUP

log "Starting polybar spawn script"

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
log "Detecting monitors..."
mapfile -t monitors < <(polybar -m | awk -F'[: x+]' '{print $5 " " $1}' | sort | awk '{print $2}')

# Select the middle one, round down
index=$(((${#monitors[@]} - 1) / 2))
export MONITOR=${monitors[$index]}
log "Selected monitor: $MONITOR (index $index of ${#monitors[@]} monitors)"

# Kill all gmail.py scripts
log "Killing existing gmail.py scripts"
pkill -f gmail.py

log "Starting polybar on monitor $MONITOR"
polybar -c "$HOME/.xmonad/polybarrc" -r primary --log=info 2>/tmp/polybar.log &
POLYBAR_PID=$!
log "Polybar started with PID $POLYBAR_PID"

# Wait for polybar and log when it exits
wait $POLYBAR_PID
EXIT_CODE=$?
log "Polybar exited with code $EXIT_CODE"

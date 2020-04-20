#!/usr/bin/env bash

# shellcheck source=polybar.bash
source "$MCF/.xmonad/polybar.bash"
# shellcheck source=solarized.bash
source "$MCF/.xmonad/solarized.bash"

StartSegment

if ael vpn is-active; then
  Add "AEL"
  Color "#ffffff" "$SolarizedMagenta"
  Action 3 "ael vpn stop"
fi

FlushSegment

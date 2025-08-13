#!/usr/bin/env bash

# shellcheck source=polybar.bash
source "$HOME/.xmonad/polybar.bash"
# shellcheck source=onedark.bash
source "$HOME/.xmonad/onedark.bash"

ICON_GOOD=" "
ICON_BAD=" "

# If homelink is not installed, do nothing
if ! hash homelink 2>/dev/null; then
  exit 0
fi

# Get status from homelink daemon
status=$(homelink status --json 2>/dev/null)

# Check if we got invalid JSON response (daemon not running or error)
if ! echo "$status" | jq . >/dev/null 2>&1; then
  StartSegment
  Add "$ICON_BAD"
  Color "$OneDarkBackground" "$OneDarkRed"
  FlushSegment
  exit 0
fi

# Extract state variables from JSON
location=$(echo "$status" | jq -r '.location // "unknown"')
internet=$(echo "$status" | jq -r '.internet // "unknown"')
vpn=$(echo "$status" | jq -r '.vpn // "unknown"')

# Determine icon and foreground color based on state variables
if [[ "$location" == "home" ]]; then
  exit 0 # Do not show anything when physically in home network
elif [[ "$internet" == "down" ]]; then
  icon=$ICON_BAD
  foreground="$OneDarkYellow"
elif [[ "$vpn" != "up" ]]; then
  icon=$ICON_BAD
  foreground="$OneDarkOrange"
else # Internet up/degraded and VPN up is considered good
  icon=$ICON_GOOD
  foreground="$OneDarkGreen"
fi

StartSegment
Add "$icon"
Color "$foreground" "$OneDarkBackground"
FlushSegment

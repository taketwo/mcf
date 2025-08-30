#!/usr/bin/env bash

# If homelink is not installed, do nothing
if ! hash homelink 2>/dev/null; then
  exit 0
fi

# shellcheck source=polybar.bash
source "$HOME/.xmonad/polybar.bash"
# shellcheck source=onedark.bash
source "$HOME/.xmonad/onedark.bash"

ICON_GOOD=" "
ICON_BAD=" "

# Get status from homelink daemon
status=$(homelink status --json 2>/dev/null)

# Helper function for jq checks
check_status() {
  echo "$status" | jq -e "$1" >/dev/null 2>&1
}

# Check if JSON is valid AND doesn't have error key
if ! check_status 'has("error") | not'; then
  StartSegment
  Add "$ICON_BAD"
  Color "$OneDarkBackground" "$OneDarkRed"
  FlushSegment
  exit 0
fi

# Determine icon and color based on status
if check_status '.location == "home"'; then
  echo ""
  exit 0 # Do not show anything when physically in home network
elif check_status '.internet == "down"'; then
  icon=$ICON_BAD
  foreground="$OneDarkYellow"
elif check_status '.vpn != "up"'; then
  icon=$ICON_BAD
  foreground="$OneDarkOrange"
else # Internet up/uncertain and VPN up is considered good
  icon=$ICON_GOOD
  foreground="$OneDarkGreen"
fi

StartSegment
Add "$icon"
Color "$foreground" "$OneDarkBackground"
FlushSegment

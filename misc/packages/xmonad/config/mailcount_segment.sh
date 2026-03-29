#!/usr/bin/env bash

# If mailcount is not installed, do nothing
if ! hash mailcount 2>/dev/null; then
  exit 0
fi

# shellcheck source=polybar.bash
source "$HOME/.xmonad/polybar.bash"
# shellcheck source=onedark.bash
source "$HOME/.xmonad/onedark.bash"

ICON=" "

if ! count=$(mailcount status 2>/dev/null); then
  StartSegment
  Add "$ICON"
  Color "$OneDarkBackground" "$OneDarkRed"
  FlushSegment
  exit 0
fi

if [[ "$count" -gt 0 ]]; then
  StartSegment
  Add "$ICON $count"
  Color "$OneDarkRed" "$OneDarkBackground"
  FlushSegment
else
  StartSegment
  Add "$ICON"
  Color "$OneDarkGrey" "$OneDarkBackground"
  FlushSegment
fi

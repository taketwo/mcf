#!/usr/bin/env bash

# shellcheck source=polybar.bash
source "$MCF/.xmonad/polybar.bash"
# shellcheck source=solarized.bash
source "$MCF/.xmonad/solarized.bash"

case $(keyboard -g) in
  "us(dvorak)") layout="DK";;
  "ru") layout="RU";;
  "us") layout="US";;
  "de") layout="DE";;
     *) layout="??";;
esac

StartSegment
Add "$layout"
Action 1 "keyboard -n"
Action 2 "keyboard -s de"
Action 3 "keyboard -s us"
Color "#ffffff" "$SolarizedYellow"

FlushSegment

#!/usr/bin/env bash

source $MCF/.xmonad/polybar.bash
source $MCF/.xmonad/solarized.bash

case `keyboard -g` in
  "us(dvorak)") layout="DK";;
  "ru") layout="RU";;
  "us") layout="US";;
  "de") layout="DE";;
     *) layout="??";;
esac

StartIndicator
Add $layout
Action 1 "keyboard -n"
Action 2 "keyboard -s de"
Action 3 "keyboard -s us"
Color "#ffffff" $SolarizedYellow

FlushIndicator

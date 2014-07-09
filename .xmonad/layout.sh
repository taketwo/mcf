#!/bin/bash

source $MCF/.xmonad/panel.bash

Start

case `$MCF/scripts/bin/keyboard -g` in
  "us(dvorak)" ) Add "DV"
    ;;
  "us" ) Add "US"
    ;;
  "ru" ) Add "RU"
    ;;
esac

Click 1 "keyboard -n"
Click 3 "keyboard -s us"
Flush

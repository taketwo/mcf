#!/bin/bash

source $MCF/.xmonad/panel.bash
source $MCF/.xmonad/solarized.bash

StartIndicator

if [[ `$MCF/scripts/bin/toggl running` == "yes" ]]; then
  Icon "toggl"
  Bg $SolarizedGreen
  Fg $SolarizedBase3
fi

FlushIndicator

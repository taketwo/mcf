#!/bin/bash

source $MCF/.xmonad/panel.bash

Start

if [[ `$MCF/scripts/bin/toggl running` == "yes" ]]; then
  Icon "toggl"
  Bg $SolarizedGreen
  Fg $SolarizedBase3
fi

Flush

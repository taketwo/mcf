#!/bin/bash

source $MCF/.xmonad/panel.bash
source $MCF/.xmonad/solarized.bash

if [ "$1" != "-p" ]; then
#######################################################################
#                           Indicator mode                            #
#######################################################################
  StartIndicator
  if [[ `$MCF/scripts/bin/toggl running` == "yes" ]]; then
    Icon "toggl"
    Bg $SolarizedGreen
    Fg $SolarizedBase3
  fi
  FlushIndicator
else
#######################################################################
#                             Popup mode                              #
#######################################################################
  # add protection from multiple instances (test ps aux | grep ...)
  StartPopup
  Add "`$MCF/scripts/bin/toggl current`"
  Fg $SolarizedBase3
  NewLine
  FlushPopup
fi


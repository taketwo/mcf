#!/bin/bash

source $MCF/.xmonad/panel.bash
source $MCF/.xmonad/solarized.bash

user="alexandrov88"
pass=`$MCF/scripts/bin/keyring-password liamg`
feed=`curl -u $user:$pass -s -sslreqd -m 5 "https://mail.google.com/mail/feed/atom"`

if [ "$1" != "-p" ]; then
#######################################################################
#                           Indicator mode                            #
#######################################################################
  count=`echo "$feed" | grep -oP "(?<=<fullcount>)(.*)(?=</fullcount>)"`
  StartIndicator
  Icon "mail"
  Add $count
  if [[ $count > 0 ]]; then
    Fg $SolarizedBase3
    Bg $SolarizedRed
  fi
  FlushIndicator
else
#######################################################################
#                             Popup mode                              #
#######################################################################
  # add protection from multiple instances (test ps aux | grep ...)
  StartPopup
  count=0
  while read -r title; do
    code=$((count+9312))
    ((count++))
    # Decode special HTML characters like ampersand
    #title=`echo " &#$code  $title" | w3m -dump -T text/html`
    Add " $count "
    Bg $SolarizedGreen
    title=`echo "$title" | w3m -dump -T text/html`
    Add " $title"
    Fg $SolarizedBase3
    NewLine
  done < <(echo "$feed" | grep -oP "(?<=<entry><title>)(.*?)(?=</title>)")
  FlushPopup
fi


#!/bin/bash

source $MCF/.xmonad/panel.bash

user="alexandrov88"
pass=`$MCF/scripts/bin/keyring-password liamg`
count=`curl -u $user:$pass -s -sslreqd -m 5 "https://mail.google.com/mail/feed/atom" | grep -o "<entry>" | wc -l`

Start

Icon "mail"
Add $count

if [[ $count > 0 ]]; then
  Fg $solarizedBase3
  Bg $solarizedRed
fi

Flush

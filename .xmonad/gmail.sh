#!/bin/bash

user="alexandrov88"
pass=""
count=`curl -u $user:$pass -s -sslreqd -m 5 "https://mail.google.com/mail/feed/atom" | grep -c "<entry>"`

solarizedRed="#dc322f"
solarizedBase3="#fdf6e3"

mail="^i(/home/sergey/.xmonad/icons/mail.xbm)"

if [[ $count > 0 ]]; then
  echo -n "^bg($solarizedRed)^fg($solarizedBase3) $mail $count ^fg()^bg()"
else
  echo -n " $mail $count "
fi

#!/bin/bash

user="alexandrov88"
pass=""
count=`curl -u $user:$pass -s -sslreqd "https://mail.google.com/mail/feed/atom" | grep -c "<entry>"`

mail="^i(/home/sergey/.xmonad/icons/mail.xbm)"

if [[ $count > 0 ]]; then
  echo -n "^fg(#ff7272)$mail^fg() $count"
else
  echo -n "$mail $count"
fi

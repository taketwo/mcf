#!/bin/bash

user="alexandrov88"
pass=""
new=`curl -u $user:$pass -s -sslreqd "https://mail.google.com/mail/feed/atom" | grep -c "<entry>"`

echo -n "^fg(\#a9a6af)^i(/home/sergey/.xmonad/icons/mail.xbm)^fg() "
if [[ $new > 0 ]]; then
  echo -n "^fg(\#ff7272)$new^fg()"
else
  echo -n "$new"
fi

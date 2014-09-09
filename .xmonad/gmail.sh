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
  count=`echo "$feed" | grep -o "<entry>" | wc -l`
  Start
  Icon "mail"
  Add $count
  if [[ $count > 0 ]]; then
    Fg $SolarizedBase3
    Bg $SolarizedRed
  fi
  Flush
else
#######################################################################
#                             Popup mode                              #
#######################################################################
  # add protection from multiple instances (test ps aux | grep ...)
  width=400
  font_pixelsize=11
  line_height=$((font_pixelsize + 4))
  lines=8
  height=$((line_height * (lines + 1)))
  bottom_gap=20
  read _ s_width <<< "$(xwininfo -root | egrep Width)"
  read _ s_height <<< "$(xwininfo -name "dzen title" | egrep Height)"
  {
  echo '^fg(#a00)GMail^fg()'
  titles=`echo "$feed" | grep -oP "(?<=<entry><title>)(.*?)(?=</title>)"`
  echo $titles
  echo '^uncollapse()'
  } | dzen2 -x $((s_width - width)) -y $s_height -w $width -l $lines -h $line_height -sa center -fn "Liberation Mono:pixelsize=$font_pixelsize" -e 'leaveslave=exit;button1=exit;button3=exit;onstart=uncollapse' -p -bg "#3c3b37" -title-name 'gmail_popup'
fi


#!/bin/bash

online ()
{
  ping -c1 8.8.8.8 >/dev/null
  if [ $? -ne 0 ] ; then
    return 1
  fi
  ping -c1 173.194.112.70 >/dev/null
  if [ $? -ne 0 ] ; then
    return 1
  fi
  ping -c1 google.com >/dev/null
  if [ $? -ne 0 ] ; then
    return 1
  fi
  return 0
}

solarizedBlue="#268bd2"
solarizedBase3="#fdf6e3"

online
if [[ $? = 0 ]] ; then
  echo -n "^bg($solarizedBlue)^fg($solarizedBase3)"
else
  echo -n "^bg()^fg()"
fi

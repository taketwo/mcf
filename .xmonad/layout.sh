#!/bin/sh

while read input ; do
  layout=`setxkbmap -print | grep xkb_symbols | awk '{print $4}' | awk -F"+" '{print $2}'`
  echo "$layout $input"
done

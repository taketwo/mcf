#!/bin/bash

rhythmbox=`/usr/bin/rhythmbox-client --print-playing --no-start --no-present`

case $rhythmbox in
  " - XFM London" )
    xfm=`python ~/.xmonad/xfm.py`
    echo -n " $xfm";;
  " - " )
    echo -n " [paused]";;
  *)
    echo -n " $rhythmbox";;
esac

# Just for reference, this piece of bash code fetches artist and song
# names from Xfm. Almost correctly.
# curl -s -m 5 "www.xfm.co.uk/london/playlist/" | tac | grep "hed -n 's,.*>\([^<]*\)<.*,\1,p' | tr '\n' '~' | sed 's/~/ - /'"

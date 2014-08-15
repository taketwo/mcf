#!/bin/bash

source $MCF/.xmonad/panel.bash

Start

if hash rhythmbox-client 2>/dev/null; then
  rhythmbox=`/usr/bin/rhythmbox-client --print-playing --no-start --no-present`
  case $rhythmbox in
    " - XFM London" )
      xfm=`python ~/.xmonad/xfm.py`
      Add "$xfm"
      ;;
    " - " )
      Add "[paused]"
      ;;
    *)
      Add "$rhythmbox"
      ;;
  esac
fi

Fg $solarizedBase3

Click 1 "rhythmbox-client --play-pause"

Flush

# Just for reference, this piece of bash code fetches artist and song
# names from Xfm. Almost correctly.
# curl -s -m 5 "www.xfm.co.uk/london/playlist/" | tac | grep "hed -n 's,.*>\([^<]*\)<.*,\1,p' | tr '\n' '~' | sed 's/~/ - /'"

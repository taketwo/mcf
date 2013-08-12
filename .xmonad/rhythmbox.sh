#!/bin/bash

rhythmbox=`/usr/bin/rhythmbox-client --print-playing --no-start --no-present`

case $rhythmbox in
  " - XFM London" )
    xfm=`curl -s -m 5 "http://rope.ixfm.fimc.net/Feeds/NowPlaying/GCap_Media/XFM_London/5893.xml" | tr '\n' ' ' | sed 's/.*title="\([^"]*\).*name="\([^"]*\).*/\2 - \1/'`
    echo -n " $xfm";;
  " - " )
    echo -n " [paused]";;
  *)
    echo -n " $rhythmbox";;
esac

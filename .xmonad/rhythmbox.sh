#!/bin/bash

rhythmbox=`/usr/bin/rhythmbox-client --print-playing --no-start --no-present`

if [[ $rhythmbox = " - XFM London" ]]; then
  xfm=`curl -s -m 5 "http://rope.ixfm.fimc.net/Feeds/NowPlaying/GCap_Media/XFM_London/5893.xml" | tr '\n' ' ' | sed 's/.*title="\([^"]*\).*name="\([^"]*\).*/\2 - \1/'`
  echo -n " $xfm"
else
  echo -n " $rhythmbox"
fi

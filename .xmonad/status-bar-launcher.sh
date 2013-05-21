#!/bin/bash

#FONT = "-*-CPMono_v07 Plain for Powerline-*-r-normal-*-11-*-*-*-*-*-*-*"
DZEN="dzen2 -x '0' -y '0' -h '14' -w '450' -ta 'l' -fn '-*-CPMono_v07 Plain for Powerline-*-r-normal-*-11-*-*-*-*-*-*-*' -bg '#3c3b37' -xs "
DZEN1=$DZEN"'1'"
DZEN2=$DZEN"'2'"
#DZEN1="sort -u"
#DZEN2="spark"
#exec &0> >(exec "dzen2")
#exec &0> tee >(sort -r)


pee <&0 "$DZEN1" "$DZEN2"
#tee <&0 >($DZEN1) >($DZEN2)

#coproc d1 { $DZEN1 ;}
#coproc d2 { $DZEN2 ;}

#PID=$!          ## you can also get the process' PID
##[[ $? -ne 0 ]]  ## perhaps it means that the command failed, please test

#while read input ; do
  #echo "$input" >&${d1[0]}
  #echo "$input" >&${d2[0]}
#done

#exec 4>&-  ## closes the fd or closes connection to the process

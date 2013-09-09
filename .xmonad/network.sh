#!/bin/bash

nm_connections ()
{
  wireless=0
  ethernet=0
  connections=`nmcli --terse --fields VPN,DEVICES,NAME con status`
  nm=()
  for con in $connections; do
    IFS=':' read -a c <<< "$con"
    case "${c[0]}" in
      yes ) nm=("${nm[@]}" "${c[2]}")
            ;;
      no  ) case "${c[1]}" in
              eth*  ) ethernet=1
                      ;;
              wlan* ) wireless=1
                      ;;
            esac
            ;;
    esac
  done
}

openvpn_connections ()
{
  openvpn=()
  kebrum=()
  connections=`service openvpn status | sed -n "s/ \* VPN '\(.*\)' is running/\1/p"`
  for c in $connections; do
    if [[ "$c" =~ "Kebrum" ]] ; then
      IFS='.' read -a name <<< "$c"
      kebrum=("${kebrum[@]}" "$name")
    else
      openvpn=("${openvpn[@]}" "$c")
    fi
  done
}

is_online ()
{
  online=1
  ping -c1 8.8.8.8 >/dev/null
  if [ $? -ne 0 ] ; then
    online=0
    return
  fi
  ping -c1 173.194.112.70 >/dev/null
  if [ $? -ne 0 ] ; then
    online=0
    return
  fi
  ping -c1 google.com >/dev/null
  if [ $? -ne 0 ] ; then
    online=0
    return
  fi
}

Start ()
{
  eval "output=' '"
}

Fg ()
{
  eval "output='^fg($1)$output^fg()'"
}

Bg ()
{
  eval "output='^bg($1)$output^bg()'"
}

Icon ()
{
  eval "output='$output^i(/home/sergey/.xmonad/icons/${1}.xbm) '"
}

Add ()
{
  eval "output='$output$1 '"
}

Flush ()
{
  echo -n "$output"
}

solarizedBlue="#268bd2"
solarizedYellow="#b58900"
solarizedBase3="#fdf6e3"

nm_connections
openvpn_connections
is_online

Start

# NetworkManager devices
if [ $wireless -eq 1 ] ; then
  Icon "wifi"
fi
if [ $ethernet -eq 1 ] ; then
  Icon "net_wired"
fi

# NetworkManager VPN connections
for c in $nm ; do
  Icon "lock"
  Add $c
done

# OpenVPN connections (non Kebrum)
for c in $openvpn ; do
  Icon "lock"
  Add $c
done

# Paint blue if online
if [ $online -eq 1 ] ; then
  Fg $solarizedBase3
  Bg $solarizedBlue
fi

Flush
Start

# OpenVPN connections (Kebrum)
for c in $kebrum ; do
  Icon "lock"
  Add $c
done

if [ "$output" != " " ] ; then
  Fg $solarizedBase3
  Bg $solarizedYellow
  Flush
fi

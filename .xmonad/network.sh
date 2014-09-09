#!/bin/bash

source $MCF/.xmonad/panel.bash
source $MCF/.xmonad/solarized.bash
source $MCF/scripts/library/version-compare.bash

nm_connections ()
{
  wireless=0
  ethernet=0
  nmcli_version=`nmcli -v | grep -Po "(?<=version ).*"`
  version-compare $nmcli_version 0.9.10.1
  if [[ $? == 2 ]]; then
    # We are dealing with an old nmcli version
    connections=`nmcli --terse --fields VPN,DEVICES,NAME con status`
  else
    connections=`nmcli --terse --fields TYPE,DEVICE,NAME con show`
  fi
  nm=()
  for con in $connections; do
    IFS=':' read -a c <<< "$con"
    case "${c[0]}" in
      yes|vpn ) nm=("${nm[@]}" "${c[2]}")
                ;;
      * )       case "${c[1]}" in
                  eth*|enp* ) ethernet=1
                              ;;
                  wl* )       wireless=1
                              ;;
                esac
                ;;
    esac
  done
}

openvpn_connections ()
{
  return
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

nm_connections
openvpn_connections
is_online

StartIndicator

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
  Fg $SolarizedBase3
  Bg $SolarizedBlue
fi

FlushIndicator
StartIndicator

# OpenVPN connections (Kebrum)
for c in $kebrum ; do
  Icon "lock"
  Add $c
done

Fg $SolarizedBase3
Bg $SolarizedYellow
FlushIndicator

#!/bin/bash

source $MCF/.xmonad/xmobar.bash
source $MCF/.xmonad/solarized.bash
source $MCF/scripts/library/version-compare.bash

nm_connections_old ()
{
  while read -r connection; do
    read -a c <<< "$connection"
    case "${c[4]}" in
      yes ) nm=("${nm[@]}" "${c[0]}")
            ;;
      no )  case "${c[2]}" in
              eth*|enp* ) ethernet=1
                          ;;
              wl* )       wireless=1
                          ;;
            esac
            ;;
    esac
  done < <(nmcli connection status)
}

nm_connections_new ()
{
  while read -r connection; do
    read -a c <<< "$connection"
    case "${c[2]}" in
      yes|vpn ) nm=("${nm[@]}" "${c[0]}")
                ;;
      * )       case "${c[3]}" in
                  eth*|enp* ) ethernet=1
                              ;;
                  wl* )       wireless=1
                              ;;
                esac
                ;;
    esac
  done < <(nmcli connection show --active)
}

nm_connections ()
{
  wireless=0
  ethernet=0
  nm=()
  nmcli_version=`nmcli -v | grep -Po "(?<=version ).*"`
  version-compare $nmcli_version 0.9.10.1
  if [[ $? == 2 ]]; then
    nm_connections_old
  else
    nm_connections_new
  fi
}

openvpn_connections_old ()
{
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

openvpn_connections_new ()
{
  connections=`systemctl status openvpn@* | grep -oP "(?<=OpenVPN connection to )(.*)"`
  for c in $connections; do
    if [[ "$c" =~ "Kebrum" ]] ; then
      IFS='.' read -a name <<< "$c"
      kebrum=("${kebrum[@]}" "$name")
    else
      openvpn=("${openvpn[@]}" "$c")
    fi
  done
}

openvpn_connections ()
{
  openvpn=()
  kebrum=()
  nmcli_version=`nmcli -v | grep -Po "(?<=version ).*"`
  version-compare $nmcli_version 0.9.10.1
  if [[ $? == 2 ]]; then
    openvpn_connections_old
  else
    openvpn_connections_new
  fi
}

is_online ()
{
  online=1
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
  Symbol "wifi"
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
  Color $SolarizedBase3 $SolarizedBlue
fi

FlushIndicator
StartIndicator

# OpenVPN connections (Kebrum)
for c in $kebrum ; do
  Icon "lock"
  Add $c
done

Color $SolarizedBase3 $SolarizedYellow
FlushIndicator

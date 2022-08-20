#!/bin/bash

source $MCF/.xmonad/xmobar.bash
source $MCF/.xmonad/solarized.bash
source $MCF/scripts/library/version-compare.bash
source $MCF/bash/555-boleh.bash

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

boleh_connections ()
{
  boleh=()
  connections=$(__boleh_active)
  for c in $connections; do
    boleh=("${boleh[@]}" "$c")
  done
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
boleh_connections
is_online

StartIndicator

# NetworkManager devices
if [ $wireless -eq 1 ] ; then
  Symbol "wifi"
fi
if [ $ethernet -eq 1 ] ; then
  Symbol "planet"
fi

# NetworkManager VPN connections
for c in $nm ; do
  Symbol "shield"
  Add $c
done

# Paint blue if online
if [ $online -eq 1 ] ; then
  Color $SolarizedBase3 $SolarizedBlue
fi

FlushIndicator
StartIndicator

# Boleh OpenVPN connections
for c in $boleh ; do
  Symbol "shield"
  Add $c
done

Color $SolarizedBase3 $SolarizedYellow
FlushIndicator

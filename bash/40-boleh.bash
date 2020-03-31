function boleh() {
  if [[ $# -eq 0 ]]; then
    __boleh_status
  else
    case $1 in
      active)
        __boleh_active
        ;;
      status)
        __boleh_status
        ;;
      import)
        __boleh_import
        ;;
      off)
        sudo systemctl stop openvpn@*
        ;;
      *) __boleh_on "$1" ;;
    esac
  fi
}

# Download and setup configuration files for Boleh VPN
function __boleh_import() {
  archive="$HOME/Downloads/taketwo_n.zip"
  tmp=/tmp/boleh
  rm -f "$archive"
  read -r -p 'Download config https://users.bolehvpn.net/download/normal/6 and press ENTER'
  if [[ ! -f $archive ]]; then
    echo "Cannot find config at expected location $archive"
  else
    echo "Removing old Boleh configurations"
    sudo rm /etc/openvpn/*Boleh.conf
    unzip -o "$archive" -d $tmp >/dev/null
    sudo cp /tmp/boleh/*{crt,key} /etc/openvpn
    echo "Importing Boleh fully routed configurations:"
    for f in "$tmp"/FullyRouted*ovpn; do
      country=$(echo "$f" | awk -F'[-.]' '{print $2}')
      if [[ $country != 'TCP' ]]; then
        echo " * $country"
        sudo cp "$tmp/FullyRouted-$country.ovpn" "/etc/openvpn/$country.Boleh.conf"
      fi
    done
  fi
}

# Turn on a Boleh connection
function __boleh_on() {
  config="$1.Boleh"
  if [ -f "/etc/openvpn/$config.conf" ]; then
    sudo systemctl start "openvpn@$config.service"
  else
    echo "Unknown connection $1"
    return 1
  fi
}

# List active Boleh connections
function __boleh_active() {
  connections=$(systemctl status openvpn@* | grep -oP "(?<= - OpenVPN connection to )([^.]+)(?=\.Boleh)")
  if [[ $connections != "" ]]; then
    for c in $connections; do
      info=$(systemctl status "openvpn@$c.Boleh.service")
      state=$(grep -oP "(?<=Active: )(\w+)" <<<"$info")
      if [[ $state == "active" ]]; then
        echo -n "$c"
      fi
    done
    echo ""
  fi
}

# Print full status of Boleh connections
function __boleh_status() {
  connections=$(__boleh_active)
  if [[ $connections != "" ]]; then
    for c in $connections; do
      info=$(systemctl status "openvpn@$c.Boleh.service")
      state=$(grep -oP "(?<=Active: )(\w+)" <<<"$info")
      echo "●  $c (state: $state)"
      ip=$(grep -oP -m 1 "(?<=^remote )([^ ]+)" "/etc/openvpn/$c.Boleh.conf")
      public_ip=$(curl ifconfig.me/ip 2>/dev/null)
      if [[ $ip =~ $public_ip ]]; then
        echo -e "   IP: \e[00;32m$ip\e[00m"
      else
        echo -e "   IP: \e[00;31m$ip\e[00m"
      fi
    done
  else
    echo "No active connections"
  fi
}

function __complete_boleh() {
  servers=$(ls /etc/openvpn/ | grep -oP "([^.]+)(?=\.Boleh\.conf)")
  local current=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=($(compgen -W "$servers active status import off" -- "$current"))
}

link_complete_function boleh

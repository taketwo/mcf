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
        echo "Stopping all OpenVPN services"
        sudo systemctl stop openvpn-client@*
        echo "Waiting 5 seconds before setting DNS address" && sleep 5
        sudo systemd-resolve --set-dns=1.1.1.1 --interface wlp3s0
        ;;
      *) __boleh_on "$1" ;;
    esac
  fi
}

# Download and setup configuration files for Boleh VPN
function __boleh_import() {
  archive="$HOME/Downloads/taketwo_n.zip"
  tmp=/tmp/boleh
  target=/etc/openvpn/client
  rm -f "$archive"
  read -r -p 'Download config https://users.bolehvpn.net/download/normal/6 and press ENTER'
  if [[ ! -f $archive ]]; then
    echo "Cannot find config at expected location $archive"
  else
    echo "Removing old Boleh configurations"
    sudo rm $target/*Boleh.conf
    unzip -o "$archive" -d $tmp >/dev/null
    sudo cp $tmp/*{crt,key} $target
    echo "Importing Boleh fully routed configurations:"
    cd $tmp || return
    for f in *TCP.ovpn; do
      location=$(echo "$f" | awk -F'[-]' 'NF==3 {printf $1"-"$2}')
      if [[ -n $location ]]; then
        echo " * $location"
        sudo cp "$f" "$target/$location.Boleh.conf"
      fi
    done
    cd $target || return
  fi
}

# Turn on a Boleh connection
function __boleh_on() {
  config="$1.Boleh"
  if [ -f "/etc/openvpn/client/$config.conf" ]; then
    echo "Starting OpenVPN service with $config config"
    sudo systemctl start "openvpn-client@$config.service"
    echo "Waiting 5 seconds before setting DNS address" && sleep 5
    sudo systemd-resolve --set-dns=172.16.1.1 --interface wlp3s0
    echo "Done"
  else
    echo "Unknown connection $1"
    return 1
  fi
}

# List active Boleh connections
function __boleh_active() {
  connections=$(systemctl status openvpn-client@* | grep -oP "(?<= - OpenVPN tunnel for )([^.]+)(?=\.Boleh)" | tr '/' '-')
  if [[ $connections != "" ]]; then
    for c in $connections; do
      info=$(systemctl status "openvpn-client@$c.Boleh.service")
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
      info=$(systemctl status "openvpn-client@$c.Boleh.service")
      state=$(grep -oP "(?<=Active: )(\w+)" <<<"$info")
      echo "●  $c (state: $state)"
      server_url=$(grep -oP -m 1 "(?<=^remote )([^ ]+)" "/etc/openvpn/client/$c.Boleh.conf")
      server_ip="$(host "$server_url" | awk '/has.*address/{print $NF; exit}')"
      public_ip=$(curl ifconfig.me/ip 2>/dev/null)
      echo -e "   Server IP: $server_ip"
      echo -e "   Public IP: $public_ip"
      if [[ $server_ip =~ $public_ip ]]; then
        echo -e "   \e[00;32mAll good\e[00m"
      else
        echo -e "   \e[00;31mHouston, we have a problem\e[00m"
      fi
    done
  else
    echo "No active connections"
  fi
}

function __complete_boleh() {
  servers=$(ls /etc/openvpn/client/ | grep -oP "([^.]+)(?=\.Boleh\.conf)")
  local current=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=($(compgen -W "$servers active status import off" -- "$current"))
}

link_complete_function boleh

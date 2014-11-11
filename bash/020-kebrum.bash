function kebrum ()
{
  if [[ $? -eq 0 ]]; then
    # Collect and print status information
    connections=`systemctl status openvpn@* | grep -oP "(?<=OpenVPN connection to )([^.]+)(?=\.Kebrum)"`
    echo "Active connections: $connections"
    for c in $connections; do
      info=`systemctl status openvpn@$c.Kebrum.TCP.service`
      echo "● $c"
      active=`grep -oP "active.*; \K(.+)(?= ago)" <<< "$info"`
      echo "  Active: $active"
      status=`grep -oP "(?<=status=)(.+)(?=\))" <<< "$info"`
      echo "  Status: $status"
      ip=`cat /etc/openvpn/$c.Kebrum.TCP.conf | grep -oP "(?<=^remote )(.*)"`
      public_ip=`curl ifconfig.me/ip 2>/dev/null`
      if [[ $ip =~ $public_ip ]]; then
        echo -e "  IP: \e[00;32m$ip\e[00m"
      else
        echo -e "  IP: \e[00;31m$ip\e[00m"
      fi
    done
  else
    case $1 in
      off ) if hash systemctl 2>/dev/null; then
              sudo systemctl stop openvpn@*
            else
              sudo service openvpn stop
            fi
            ;;
      *   ) config="$1.Kebrum.TCP"
            if [ -f "/etc/openvpn/$config.conf" ]; then
              if hash systemctl 2>/dev/null; then
                sudo systemctl start openvpn@$config.service
              else
                sudo service openvpn start $config
              fi
            fi
    esac
  fi
}

function __complete_kebrum ()
{
  servers=`ls /etc/openvpn/ | grep -oP "([^.]+)(?=\.Kebrum)"`
  local current=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=($(compgen -W "$servers off" -- $current))
}

link_complete_function kebrum

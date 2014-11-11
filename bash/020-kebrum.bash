function kebrum ()
{
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
}

function __complete_kebrum ()
{
  servers=`ls /etc/openvpn/ | grep -oP "([^.]+)(?=\.Kebrum)"`
  local current=${COMP_WORDS[COMP_CWORD]}
  COMPREPLY=($(compgen -W "$servers off" -- $current))
}

link_complete_function kebrum

function kebrum ()
{
  case $1 in
    off ) sudo service openvpn stop
          ;;
    *   ) config="$1.Kebrum.TCP"
          if [ -f "/etc/openvpn/$config.conf" ]; then
            sudo service openvpn start $config
          fi
  esac
}

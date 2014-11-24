function msync ()
{
  # Two modes:
  #  * precision --> phone
  #  * phone     --> x201
  if [[ ! $HOSTNAME == "x201" && ! $HOSTNAME == "precision" ]]; then
    echo "This function only works on x201 and precision hosts"
    return
  fi
  if [[ $# -ne 1 || $1 -le 0 || $1 -ge 256 ]]; then
    echo "Need a single argument: last octet of the target IP address"
    return
  fi
  host_ip=`ip addr | grep 'state UP' -A2 | tail -n1 | awk '{print $2}' | cut -f1  -d'/'`
  phone_ip=`sed "s/\([0-9]*\.[0-9]*\.[0-9]*\.\)[0-9]*/\1$1/g" <<< "$host_ip"`
  if [[ $HOSTNAME == "x201" ]]; then
    rsync -avz -e 'ssh -p 2222' --size-only --progress --delete \
      root@$phone_ip:sdcard1/Music/ /home/sergey/Music
  else
    rsync -avz -e 'ssh -p 2222' --size-only --progress --delete \
      /media/Files/Music/ root@$phone_ip:sdcard1/Music
  fi
}


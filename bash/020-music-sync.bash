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
  if [[ $HOSTNAME -eq "x201" ]]; then
    rsync -avz -e 'ssh -p 2222' --size-only --progress --delete \
      root@10.20.121.$1:sdcard1/Music/ /home/sergey/Music
  else
    rsync -avz -e 'ssh -p 2222' --size-only --progress --delete \
      /media/Files/Music/ root@192.168.0.$1:sdcard1/Music
  fi
}


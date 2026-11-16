# We don't want to pay 20 ms on every Bash shell startup, so we cache the outputs
starship_init_script="/tmp/starship_init.bash"
if [[ ! -s $starship_init_script ]]; then
  exec 9>/tmp/starship_init.lock
  flock 9
  if [[ ! -s $starship_init_script ]]; then
    tmp=$(mktemp /tmp/starship_init.XXX.bash)
    if starship init bash --print-full-init >"$tmp" && [[ -s $tmp ]]; then
      mv "$tmp" "$starship_init_script"
    else
      rm -f "$tmp"
    fi
  fi
  flock -u 9
  exec 9>&-
fi
if [[ -s $starship_init_script ]]; then
  # shellcheck disable=SC1090
  source "$starship_init_script"
fi

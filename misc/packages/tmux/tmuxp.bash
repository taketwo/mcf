# Generate and cache completions
# Make sure there is a) single writer and b) it does not create an empty file
tmuxp_complete_script="/tmp/tmuxp-complete.bash"
if [[ ! -s $tmuxp_complete_script ]]; then
  exec 8>/tmp/tmuxp-complete.lock
  flock 8
  if [[ ! -s $tmuxp_complete_script ]]; then
    tmp=$(mktemp /tmp/tmuxp-complete.XXX.bash)
    if "$(uv tool dir)/tmuxp/bin/python" -m shtab --shell=bash -u tmuxp.cli.create_parser >"$tmp" && [[ -s $tmp ]]; then
      mv "$tmp" "$tmuxp_complete_script"
    else
      rm -f "$tmp"
    fi
  fi
  flock -u 8
  exec 8>&-
fi
if [[ -s $tmuxp_complete_script ]]; then
  # shellcheck disable=SC1090
  source "$tmuxp_complete_script"
fi

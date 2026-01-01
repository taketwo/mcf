# Generate and cache completions
tmuxp_complete_script="/tmp/tmuxp-complete.bash"
if [[ ! -f $tmuxp_complete_script ]]; then
  tmp=$(mktemp "/tmp/tmuxp-complete.XXX.bash")
  uvx --quiet --with tmuxp shtab --shell=bash -u tmuxp.cli.create_parser >"$tmp"
  mv "$tmp" $tmuxp_complete_script
fi
source $tmuxp_complete_script

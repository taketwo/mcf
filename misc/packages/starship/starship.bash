# We don't want to pay 20 ms on every Bash shell startup, so we cache the outputs
starship_init_script="/tmp/starship_init.bash"
if [[ ! -f $starship_init_script ]]; then
  starship init bash --print-full-init >$starship_init_script
fi
source $starship_init_script

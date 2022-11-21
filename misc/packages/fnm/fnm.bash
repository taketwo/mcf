# We don't want to pay 25 ms on every Bash shell startup, so we cache the outputs

fnm_complete_script="/tmp/fnm-complete.bash"
if [[ ! -f $fnm_complete_script ]]; then
  fnm completions --shell bash > $fnm_complete_script
fi
source $fnm_complete_script

fnm_env_script="/tmp/fnm-env.bash"
if [[ ! -f $fnm_env_script ]]; then
  fnm env > $fnm_env_script
fi
source $fnm_env_script

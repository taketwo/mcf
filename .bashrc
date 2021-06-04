# If not running interactively, source .profile and return
if [[ $- != *i* ]]; then
  . ${HOME}/.profile
  return
fi

# Enable programmable completion
# It is important to source it here:
#   * Not from any of the scripts in bash/ because when sourced that way the
#     variables in bash_completion that are supposed to be global become local
#     to the timed_source() function. This leads to problems with completion.
#     See: https://github.com/Bash-it/bash-it/issues/1245#issuecomment-441285679
#   * Not after any of the scripts in bash/ because this may make their
#     completions preferable to bash_completion completions.
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
  . /etc/bash_completion
fi

[[ $TMUX = "" ]] && export TERM='xterm-256color'

BASH_TIME_STARTUP=${BASH_TIME_STARTUP:-1}

if [[ $BASH_TIME_STARTUP == 1 ]]; then
  # Track the time it takes to load each configuration file
  bash_times_file="/tmp/bashtimes.$$"
  echo -n >"$bash_times_file"
fi

timed_source() {
  local file="$1"
  local before=$(date +%s%N)
  source "$file"
  local after=$(date +%s%N)
  echo "$file $before $after" >>"$bash_times_file"
}

GLOBAL_DIR="$MCF/bash/global"
LOCAL_DIR="$HOME/.local/bash"

scripts=()
for dir in "$GLOBAL_DIR" "$LOCAL_DIR"; do
  if [[ -d $dir ]]; then
    for conf_file in "$dir"/*; do
      if [[ -f $conf_file ]]; then
        scripts+=($(basename $conf_file))
      fi
    done
  fi
done

sorted=($(printf '%s\n' "${scripts[@]}" | sort))
for conf_file in ${sorted[@]}; do
  local_conf="$LOCAL_DIR/$conf_file"
  if [[ -f "$local_conf" ]]; then
    timed_source "$local_conf"
    shared_conf="$MCF/bash/$conf_file"
    if [[ -f "$shared_conf" ]]; then
      timed_source "$shared_conf"
    fi
  fi
  global_conf="$GLOBAL_DIR/$conf_file"
  if [[ -f "$global_conf" ]]; then
    timed_source "$global_conf"
  fi
done

unset scrits
unset sorted
unset timed_source
unset bash_times_file
unset BASH_TIME_STARTUP

# Source .bashrc.local, if present
if [ -f "${HOME}/.bashrc.local" ]; then
  . "${HOME}/.bashrc.local"
fi

# Remove Ctrl-C binding, use Ctrl-J instead
# We do this in the very end of initialization sequence to avoid weird errors while
# entering into pipenv shell, which (supposedly) sends Ctrl-J signal at some point
stty intr ^J

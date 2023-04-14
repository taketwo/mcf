alias la='lsd -lA'

alias t1='tree -L 1'
alias t2='tree -L 2'

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
alias ........="cd ../../../../../../.."

# Change directory using pushd (pushes the current directory onto a history stack)
# If the cd argument is a file instead of a directory it will open it for editing.
function cd() {
  if [ "$#" = "0" ]; then
    pushd "$HOME" >/dev/null || return
  elif [ -f "$1" ]; then
    $EDITOR "$1"
  else
    pushd "$1" >/dev/null || return
  fi
}

# Change back the directory (pop the top directory from a history stack)
# Accepts a number as an argument to step back n directories at a time.
function bd() {
  if [ "$#" = "0" ]; then
    popd >/dev/null || return
  else
    for i in $(seq "$1"); do
      popd >/dev/null || return
    done
  fi
}

# Change directory to the newest subdirectory
function cdn() {
  newest=$(find . -mindepth 1 -maxdepth 1 -type d -exec stat --printf="%Y\\t%n\\n" {} \; | sort -n -r | head -1 | cut -f2'')
  cd "$newest" || return
}

# Change directory to the oldest subdirectory
function cdo() {
  oldest=$(find . -mindepth 1 -maxdepth 1 -type d -exec stat --printf="%Y\\t%n\\n" {} \; | sort -n | head -1 | cut -f2'')
  cd "$oldest" || return
}

# Change directory to the directory of a file
function cdf() {
  if [[ -e $1 && ! -d $1 ]]; then
    cd "$(dirname "$1")" || return
  else
    cd "$1" || return
  fi
}

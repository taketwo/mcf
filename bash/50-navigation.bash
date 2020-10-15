alias la='ls -lahN'

alias t1='tree -L 1'
alias t2='tree -L 2'

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
alias ........="cd ../../../../../../.."

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

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

# Change directory to the directory of a file
function cdf ()
{
  if [[ -e $1 && ! -d $1 ]]; then
    cd "$(dirname "$1")" || return
  else
    cd "$1" || return
  fi
}

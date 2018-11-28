function mkd () { mkdir -p "$@" && eval cd "\"\$$#\""; }
alias la='ls -lahN'
alias t1='tree -L 1'
alias t2='tree -L 2'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
function cdf ()
{
  if [[ -e $1 && ! -d $1 ]]; then
    cd "$(dirname "$1")"
  else
    cd "$1"
  fi
}

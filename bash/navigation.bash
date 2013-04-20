function mkd () { mkdir -p "$@" && eval cd "\"\$$#\""; }
alias la='ls -lah'
alias t1='tree -L 1'
alias t2='tree -L 2'
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."

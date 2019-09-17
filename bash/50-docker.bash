alias docker-none-list='docker images -f "dangling=true"'
alias docker-none-remove='docker rmi $(docker images -f "dangling=true" -q)'

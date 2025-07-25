agent_socket=$(gpgconf --list-dirs | grep agent-socket | awk -F ":" '{ print $2 }')
export SSH_AUTH_SOCK="$agent_socket.ssh"
export GPG_TTY="$(tty)"

function gpg-agent-restart() {
  gpgconf --kill gpg-agent
  gpgconf --launch gpg-agent
  gpg-connect-agent updatestartuptty /bye
  echo "GPG agent restarted and reloaded"
}

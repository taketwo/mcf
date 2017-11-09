# apt-get aliases
alias agu='sudo apt-get update && sudo apt-get upgrade'
alias agi='sudo apt-get install'
alias agr='sudo apt-get remove'

# dpkg aliases
alias dgrep='dpkg -l | grep -i'

# yaourt aliases
alias yu='yaourt -Syua'
alias yi='yaourt -Sa'
alias yr='yaourt -Rc'

# nix aliases
alias ni='nix-env -i'
alias nu='nix-channel --update && nix-env -u'
alias nr='nix-env -e'

DISTRO=`cat /etc/*-release | grep -Po '(?<=DISTRIB_ID=)(.*)'`
if [[ $DISTRO == "Arch" ]]; then
  alias file-from-package='pkgfile'
  alias files-in-package='pacman -Ql'
else
  alias file-from-package='apt-file search'
  alias files-in-package='dpkg-query -L'
fi

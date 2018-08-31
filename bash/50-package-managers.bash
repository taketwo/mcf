# apt-get aliases
if hash apt-get 2>/dev/null; then
  alias agu='sudo apt-get update && sudo apt-get upgrade'
  alias agi='sudo apt-get install'
  alias agr='sudo apt-get remove'
fi

# dpkg aliases
if hash dpkg 2>/dev/null; then
  alias dgrep='dpkg -l | grep -i'
fi

# yaourt aliases
if hash yaourt 2>/dev/null; then
  alias yu='yaourt -Syua'
  alias yi='yaourt -Sa'
  alias yr='yaourt -Rc'
fi

# nix aliases
if hash nix-env 2>/dev/null; then
  alias ni='nix-env -i'
  alias nu='nix-channel --update && nix-env -u'
  alias nr='nix-env -e'
  alias ngc='nix-collect-garbage -d'
  alias nix-list-installed='nix-env -qs'
fi

DISTRO=$(cat /etc/*-release | grep -Po '(?<=DISTRIB_ID=)(.*)')
if [[ $DISTRO == "Arch" ]]; then
  alias file-from-package='pkgfile'
  alias files-in-package='pacman -Ql'
else
  alias file-from-package='apt-file search'
  alias files-in-package='dpkg-query -L'
fi

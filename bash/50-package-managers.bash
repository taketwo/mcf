# apt-get aliases
if hash apt-get 2>/dev/null; then
  alias agu='sudo apt-get update && sudo apt-get upgrade'
  alias agi='sudo apt-get install'
  alias agr='sudo apt-get remove'
  alias aar='sudo apt autoremove'
fi

# dpkg aliases
if hash dpkg 2>/dev/null; then
  alias dgrep='dpkg -l | grep -i'
fi

# nix aliases
if hash nix-env 2>/dev/null; then
  alias ni='nix-env -i'
  alias nu='nix-channel --update && nix-env -u'
  alias nr='nix-env -e'
  alias ngc='nix-collect-garbage -d'
  alias nli='nix-env -qs'
  function nlf() {
    tree "$(nix-build '<nixpkgs>' -A "$0")"
  }
fi

DISTRO=$(cat /etc/*-release | grep -Po '(?<=DISTRIB_ID=)(.*)')
if [[ $DISTRO == "Arch" ]]; then
  alias file-from-package='pkgfile'
  alias files-in-package='pacman -Ql'
else
  alias file-from-package='apt-file search'
  alias files-in-package='dpkg-query -L'
fi

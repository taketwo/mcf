# apt-get aliases
if hash apt-get 2>/dev/null; then
  alias agu='sudo apt-get update && sudo apt-get upgrade'
  alias agi='sudo apt-get install'
  alias agr='sudo apt-get remove'
  alias aar='sudo apt autoremove'
  alias file-from-package='apt-file search'
fi

# dpkg aliases
if hash dpkg 2>/dev/null; then
  alias dgrep='dpkg -l | grep -i'
  alias files-in-package='dpkg-query -L'
fi

# nix aliases
if hash nix-env 2>/dev/null; then
  alias ni='nix-env -i'
  alias nu='nix-channel --update && nix-env -u'
  alias nr='nix-env -e'
  alias ngc='nix-collect-garbage -d'
  alias nli='nix-env -qs'
  function nlf() {
    tree "$(nix-build '<nixpkgs>' -A "$1")"
  }
fi

# npm aliases (through fnm)
if hash fnm 2>/dev/null; then
  alias npmi='fnm exec --using default npm install -g'
  alias npml='fnm exec --using default npm list -g --depth=0'
  alias npmr='fnm exec --using default npm uninstall -g'
fi

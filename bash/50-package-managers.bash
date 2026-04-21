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
  function nu() {
    local tmpfile
    tmpfile=$(mktemp)

    { nix-channel --update && nix-env -u; } 2>&1 | tee "$tmpfile"
    local exit_code=${PIPESTATUS[0]}

    local upgrades
    upgrades=$(grep '^upgrading ' "$tmpfile")
    rm -f "$tmpfile"

    # cargo aliased to cargo-3ds by nixpkgs: mpm reinstalls real cargo directly
    local cargo_note=""
    if echo "$upgrades" | grep -q "^upgrading 'cargo-[0-9].*' to 'cargo-3ds-"; then
      local cargo_old_ver mpm_out cargo_new_ver
      cargo_old_ver=$(echo "$upgrades" | grep "^upgrading 'cargo-[0-9]" | grep -Po "(?<=upgrading 'cargo-)[^']+")
      mpm_out=$(mpm "nix: cargo" 2>&1)
      cargo_new_ver=$(echo "$mpm_out" | grep -Po "(?<=installing 'cargo-)[^']+")
      if [[ -n "$cargo_new_ver" && "$cargo_new_ver" != "$cargo_old_ver" ]]; then
        cargo_note="  cargo: $cargo_old_ver → $cargo_new_ver  [reinstalled after cargo-3ds drift]"
      fi
    fi

    if [[ -n "$upgrades" || -n "$cargo_note" ]]; then
      local count old new name new_name old_ver new_ver
      count=$(echo "$upgrades" | grep -v "^upgrading 'cargo-[0-9].*' to 'cargo-3ds-" | grep -c '^upgrading ')
      echo
      echo "=== Nix update complete, $count package(s) upgraded ==="
      while IFS= read -r line; do
        old=$(echo "$line" | grep -Po "(?<=upgrading ')[^']+")
        new=$(echo "$line" | grep -Po "(?<=to ')[^']+")
        # shellcheck disable=SC2001
        name=$(echo "$old" | sed 's/-[0-9].*$//')
        # shellcheck disable=SC2001
        new_name=$(echo "$new" | sed 's/-[0-9].*$//')
        old_ver="${old#"${name}-"}"
        new_ver="${new#"${new_name}-"}"
        # cargo drift line replaced by cargo_note below
        [[ "$old" == cargo-[0-9]* && "$new" == cargo-3ds-* ]] && continue
        if [[ "$name" == "$new_name" ]]; then
          echo "  $name: $old_ver → $new_ver"
        else
          echo "  $name ($old_ver) → $new_name ($new_ver)  [DRIFT]"
        fi
      done <<<"$upgrades"
      [[ -n "$cargo_note" ]] && echo "$cargo_note"
    fi

    return "$exit_code"
  }
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

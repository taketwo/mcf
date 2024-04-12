# Setup forgit
# Do not use built-in aliases, we will put forgit into git subcommands
export FORGIT_NO_ALIASES=1
# Enable copying commit hashes to clipboard
export FORGIT_COPY_CMD='xsel --clipboard --input'
# See: https://github.com/wfxr/forgit#custom-options for other configuration possibilities
# shellcheck source=/dev/null
source "$MCF/external/forgit/forgit.plugin.sh"

alias la='lsd -lA'

alias t1='tree -L 1'
alias t2='tree -L 2'

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias ......="cd ../../../../.."
alias .......="cd ../../../../../.."
alias ........="cd ../../../../../../.."

# Change directory using pushd (pushes the current directory onto a history stack)
# If the cd argument is a file instead of a directory it will open it for editing.
function cd() {
  if [ "$#" = "0" ]; then
    pushd "$HOME" >/dev/null || return
  elif [ -f "$1" ]; then
    $EDITOR "$1"
  else
    pushd "$1" >/dev/null || return
  fi
}

# Change back the directory (pop the top directory from a history stack)
# Accepts a number as an argument to step back n directories at a time.
function bd() {
  if [ "$#" = "0" ]; then
    popd >/dev/null || return
  else
    for i in $(seq "$1"); do
      popd >/dev/null || return
    done
  fi
}

# Change directory to the newest subdirectory
function cdn() {
  newest=$(find . -mindepth 1 -maxdepth 1 -type d -exec stat --printf="%Y\\t%n\\n" {} \; | sort -n -r | head -1 | cut -f2'')
  cd "$newest" || return
}

# Change directory to the oldest subdirectory
function cdo() {
  oldest=$(find . -mindepth 1 -maxdepth 1 -type d -exec stat --printf="%Y\\t%n\\n" {} \; | sort -n | head -1 | cut -f2'')
  cd "$oldest" || return
}

# Change directory to the directory of a file
function cdf() {
  if [[ -e $1 && ! -d $1 ]]; then
    cd "$(dirname "$1")" || return
  else
    cd "$1" || return
  fi
}

# Copy with progress
cpp() {
  if [ $# -lt 2 ]; then
    echo "Usage: cpp SOURCE... DEST"
    return 1
  fi

  local dest="${@: -1}"         # last arg is destination
  local sources=("${@:1:$#-1}") # all but last are sources

  # rsync options used:
  # -a   : archive mode (recursive, preserve permissions, symlinks, timestamps, etc.)
  # -h   : human-readable sizes (e.g. 1.1K, 2.3M)
  # --info=progress2 : show a single aggregated progress bar instead of per-file updates
  # --inplace        : write updated data directly to the destination file
  # --no-whole-file  : avoid rewriting entire files unnecessarily (faster for big files)
  rsync -a -h --info=progress2 --inplace --no-whole-file \
    -- "${sources[@]}" "$dest"
}

# Move with progress
mvp() {
  if [ $# -lt 2 ]; then
    echo "Usage: mvp SOURCE... DEST"
    return 1
  fi

  local dest="${@: -1}"         # last arg is destination
  local sources=("${@:1:$#-1}") # all but last are sources

  # Check if source and destination are on the same filesystem.
  # If they are, use native mv (much faster, just updates inodes).
  if [ $(df -P "${sources[0]}" | tail -1 | awk '{print $1}') = \
    $(df -P "$dest" | tail -1 | awk '{print $1}') ]; then
    command mv -- "${sources[@]}" "$dest"
    return $?
  fi

  # rsync options used (first pass):
  # -a   : archive mode (recursive + preserve attributes)
  # -h   : human-readable numbers
  # --info=progress2 : show overall progress
  # --inplace        : write directly to destination file
  # --no-whole-file  : avoid rewriting entire files unnecessarily
  # --remove-source-files : remove each file after successful copy
  rsync -a -h --info=progress2 --inplace --no-whole-file \
    --remove-source-files -- "${sources[@]}" "$dest"

  # Second pass with:
  # --delete : remove empty source directories left behind
  rsync -a -h --delete -- "${sources[@]}" "$dest"
}

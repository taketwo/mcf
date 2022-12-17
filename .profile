# A few bits of information about how the combination of .profile/.bashrc/.bash_profile
# works, in case I need to understand it one day again.
#
# .profile
#   - POSIX syntax
#   - First section sets up environment, will be run always
#   - Second section (currently not present) may print or exec, but should be conditional,
#     only for interactive shells
#   - Do not source .bashrc
#
# .bash_profile
#   - Source .profile
#   - Source .bashrc if interactive
#
# .bashrc
#   - Source .profile if not interactive and return
#
# References:
#   - What goes where (pam, profile, rc, etc), a very comprehensive answer:
#     https://unix.stackexchange.com/a/88266/55482
#   - Another good answer:
#     https://stackoverflow.com/a/903213/1525865

#---------------------------------------------------------------------#
#                               Locale                                #
#---------------------------------------------------------------------#

export LANGUAGE=en
export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_PAPER=en_US.UTF-8
export LC_NAME=en_US.UTF-8
export LC_ADDRESS=en_US.UTF-8
export LC_TELEPHONE=en_US.UTF-8
export LC_MEASUREMENT=en_US.UTF-8
export LC_IDENTIFICATION=en_US.UTF-8
export KEYBOARD_LAYOUTS="us(dvorak),ru"

#---------------------------------------------------------------------#
#                                Apps                                 #
#---------------------------------------------------------------------#

export EDITOR=nvim
export BROWSER=browser
export PAGER=less
export MANPAGER=${PAGER}
export NOSPLASH=1
export NOWELCOME=1

#---------------------------------------------------------------------#
#                                 MCF                                 #
#---------------------------------------------------------------------#

if [ -d "${HOME}/.mcf" ]; then
  export MCF="${HOME}/.mcf"
  export PATH="${MCF}/bin:${PATH}"
  export PYTHONPATH="${MCF}/scripts/library:${PYTHONPATH}"
fi

#---------------------------------------------------------------------#
#                                 Nix                                 #
#---------------------------------------------------------------------#

# Source config if it exists and was not sourced already
# Note that Nix config requires USER environment variable to be set
# This may not be the case in some situations, so we explicitly set it
if [ -f "${HOME}/.nix-profile/etc/profile.d/nix.sh" ] &&  [ -z "${NIX_PATH}" ]; then
  USER=$(whoami) . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
fi

# Add share to XDG_DATA_DIRS such that bash completion, applications, etc are picked up
if [ -n "${NIX_PATH}" ]; then
  export XDG_DATA_DIRS="$HOME/.nix-profile/share:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
fi

# Set LOCALE_ARCHIVE to point to system's locale-archive to avoid troubles with setlocale
export LOCALE_ARCHIVE=/usr/lib/locale/locale-archive

#---------------------------------------------------------------------#
#                                Misc                                 #
#---------------------------------------------------------------------#

# Local bin directory
[ -d "${HOME}/.local/bin" ] && PATH="${HOME}/.local/bin:${PATH}"

# Load Git secrets
if [ -f "${HOME}/.config/git/secrets" ]; then
  . "${HOME}/.config/git/secrets"
fi

# Cabal bin directory
[ -d "${HOME}/.cabal/bin" ] && PATH="${HOME}/.cabal/bin:${PATH}"

#---------------------------------------------------------------------#
#                               Python                                #
#---------------------------------------------------------------------#

# Pyenv
export PYENV_ROOT="${HOME}/.pyenv"
if [ -d "${PYENV_ROOT}" ]; then
  export PATH="${PYENV_ROOT}/bin:${PATH}"
  # Setup the shims path and rehash shims
  eval "$(pyenv init --path)"
fi

# Python virtual environments
export WORKON_HOME="${HOME}/.virtualenvs"
mkdir -p "${WORKON_HOME}"

# Store Pipenv virtual environments inside project
export PIPENV_VENV_IN_PROJECT=1

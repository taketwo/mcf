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

export EDITOR=vim
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
#                                Misc                                 #
#---------------------------------------------------------------------#

# Local bin directory
[ -d "${HOME}/.local/bin" ] && PATH="${HOME}/.local/bin:${PATH}"

# Nix: source config if it exists and was not sourced already
if [ -f "${HOME}/.nix-profile/etc/profile.d/nix.sh" ] &&  [ -z ${NIX_PATH} ]; then
  . "${HOME}/.nix-profile/etc/profile.d/nix.sh"
fi

# Load secret configuration settings
if [ -f "${HOME}/.secrets" ]; then
  . "${HOME}/.secrets"
fi

#---------------------------------------------------------------------#
#                               Python                                #
#---------------------------------------------------------------------#

# Pyenv
export PYENV_ROOT="${HOME}/.pyenv"
[ -d "${PYENV_ROOT}" ] && export PATH="${PYENV_ROOT}/bin:${PATH}"

# Python virtual environments
export WORKON_HOME="${HOME}/.virtualenvs"
mkdir -p ${WORKON_HOME}

# Store Pipenv virtual environments inside project
export PIPENV_VENV_IN_PROJECT=1

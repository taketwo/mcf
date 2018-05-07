export PYENV_ROOT="$HOME/.pyenv"

if [[ -s "$NVM_DIR/nvm.sh" ]]; then
  source "$NVM_DIR/nvm.sh"
fi

if [[ -d $PYENV_ROOT ]]; then
  pathprepend $PYENV_ROOT/bin
fi

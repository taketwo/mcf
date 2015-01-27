export WORKON_HOME=~/.virtualenvs
mkdir -p $WORKON_HOME

if [ -f /etc/bash_completion.d/virtualenvwrapper ]; then
  source /etc/bash_completion.d/virtualenvwrapper
else
  source /usr/bin/virtualenvwrapper_lazy.sh
fi

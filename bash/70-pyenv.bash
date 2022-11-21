# The bash-independent part of the setup was performed in .profile
# We don't want to pay 30 ms on every Bash shell startup, so we cache the outputs
pyenv_init_script="/tmp/pyenv_init.bash"
if [[ ! -f $pyenv_init_script ]]; then
  pyenv init - --no-rehash > $pyenv_init_script
fi
source $pyenv_init_script

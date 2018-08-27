# The bash-independent part of the setup was performed in .profile
if command -v pipenv 1>/dev/null 2>&1; then
  eval "$(pipenv --completion)"
fi

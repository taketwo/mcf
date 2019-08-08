set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
let g:python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
set inccommand=nosplit
source ~/.vimrc

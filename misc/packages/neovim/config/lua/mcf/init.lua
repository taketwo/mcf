vim.g.python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
vim.g.node_host_prog = '$FNM_DIR/aliases/node17/bin/neovim-node-host'

require('mcf.config.options')
require('mcf.config.lazy')
require('mcf.config.keymaps')
require('mcf.config.autocmds')
require('mcf.config.abbreviations')

require('mcf.globals')

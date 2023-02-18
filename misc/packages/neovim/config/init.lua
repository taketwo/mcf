require("config.options")

vim.g.python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
vim.g.node_host_prog = '$FNM_DIR/aliases/node17/bin/neovim-node-host'

require("config.lazy")
require("config.keymaps")
require("config.autocmds")
require("config.abbreviations")

require("config.options")

vim.g.python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
vim.g.node_host_prog = '$FNM_DIR/aliases/node17/bin/neovim-node-host'

require("config.lazy")
require("config.keymaps")
require("config.autocmds")

-- TODO: Find a better place for these
vim.cmd.abbreviate("ie", "i.e.")
vim.cmd.abbreviate("eg", "e.g.")

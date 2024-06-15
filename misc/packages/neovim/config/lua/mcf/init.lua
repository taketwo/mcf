vim.g.python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
vim.g.node_path = vim.fn.expand('$FNM_DIR') .. '/aliases/node-nvim/bin'
vim.g.node_host_prog = vim.g.node_path .. '/neovim-node-host'

require('mcf.config.options')
require('mcf.config.lazy')
require('mcf.config.keymaps')
require('mcf.config.autocmds')
require('mcf.config.abbreviations')

require('mcf.globals')

-- Setup formatting on VeryLazy event
-- TODO: Perhaps there are other setup/configuration calls that can be postponed until VeryLazy event.
-- Decide where is the right place to register such callbacks.
LazyVim.on_very_lazy(function()
  LazyVim.format.setup()
  LazyVim.root.setup()
end)

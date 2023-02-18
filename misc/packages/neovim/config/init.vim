" Misc {{{

  " %% expands to the full path of the directory that contains the current file
  cabbr <expr> %% expand('%:p:h')
  abbr ie i.e.
  abbr eg e.g.

" }}}

lua require("config.options")

let g:python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
let g:node_host_prog = '$FNM_DIR/aliases/node17/bin/neovim-node-host'

lua require("config.lazy")

lua require("config.keymaps")
lua require("config.autocmds")

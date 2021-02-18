if has('nvim')
    nnoremap <Leader>.. <cmd>Telescope find_files<cr>
    nnoremap <Leader>.b <cmd>Telescope buffers<cr>
    lua <<EOF
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ["<A-t>"] = actions.move_selection_next,
        ["<A-c>"] = actions.move_selection_previous,
      },
    }
  }
}
EOF
endif

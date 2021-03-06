if has('nvim')
    nnoremap <Leader>.. <cmd>Telescope find_files find_command=rg,--ignore,--hidden,--files<cr>
    nnoremap <Leader>.b <cmd>Telescope buffers<cr>
    nnoremap <Space> <cmd>Telescope buffers<cr>
    lua <<EOF
local actions = require('telescope.actions')
require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ["<A-t>"] = actions.move_selection_next,
        ["<A-c>"] = actions.move_selection_previous,
        ["<A-n>"] = actions.preview_scrolling_down,
        ["<A-h>"] = actions.preview_scrolling_up,
        ["<Esc>"] = actions.close,
      },
    }
  }
}
EOF
endif

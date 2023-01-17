require('which-key').register({
  ['<M-h>'] = { '<cmd>TmuxNavigateLeft<cr>', 'Go to left window or pane' },
  ['<M-n>'] = { '<cmd>TmuxNavigateRight<cr>', 'Go to right window or pane' },
  ['<M-c>'] = { '<cmd>TmuxNavigateUp<cr>', 'Go to upper window or pane' },
  ['<M-t>'] = { '<cmd>TmuxNavigateDown<cr>', 'Go to lower window or pane' },
})

vim.g.tmux_navigator_no_mappings = 1

local todo_comments = require('todo-comments')

todo_comments.setup({})

vim.keymap.set('n', ']t', todo_comments.jump_next, { desc = 'Next todo comment' })
vim.keymap.set('n', '[t', todo_comments.jump_prev, { desc = 'Previous todo comment' })
vim.api.nvim_set_keymap(
  'n',
  '<Leader>.t',
  ':TodoTelescope<CR>',
  { noremap = true, silent = true, desc = 'Show todo comments in Telescope' }
)

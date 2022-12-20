require('which-key').register({
  ['<Leader>'] = {
    g = {
      name = 'Git',
      c = { '<cmd>Magit<cr>', 'Commit' },
    },
  },
})

vim.g.magit_jump_next_hunk = '>'
vim.g.magit_jump_prev_hunk = '<'

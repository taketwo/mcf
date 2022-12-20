require('which-key').register({
  ['<Leader>'] = {
    g = {
      name = 'Git',
      b = { '<cmd>Git blame<cr>', 'Blame' },
    },
  },
})

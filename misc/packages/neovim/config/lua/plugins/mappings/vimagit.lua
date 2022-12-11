require('which-key').register{
  ['<Leader>'] = {
    g = {
      name = 'Git',
      c = { '<cmd>Magit<cr>', 'Commit' },
    },
  },
}

require('which-key').register{
  ['<Leader>s'] = {
    name = '+Sideways',
    h = { '<cmd>SidewaysLeft<cr>', 'Push left' },
    n = { '<cmd>SidewaysRight<cr>', 'Push right' },
    H = { '<cmd>SidewaysJumpLeft<cr>', 'Jump left' },
    N = { '<cmd>SidewaysJumpRight<cr>', 'Jump right' },
  },
}

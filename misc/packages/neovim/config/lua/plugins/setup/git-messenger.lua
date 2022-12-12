require('which-key').register{
  ['<Leader>'] = {
    g = {
      name = 'Git',
      m = { '<cmd>GitMessenger<cr>', 'View commit message' },
    },
  },
}

-- Disable default mapping such that we can create our own one through Which-Key
vim.g.git_messenger_no_default_mappings = 1

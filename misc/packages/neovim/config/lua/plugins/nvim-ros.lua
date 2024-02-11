return {
  {
    'taketwo/nvim-ros',
    dependencies = {
      { 'nvim-telescope/telescope.nvim' },
      { 'taketwo/vim-ros' },
      {
        'folke/which-key.nvim',
        opts = {
          defaults = {
            ['<Leader>r'] = { name = 'ROS' },
          },
        },
      },
    },
    keys = {
      { '<Leader>re', '<cmd>Telescope ros ed<cr>', desc = 'Open file in package' },
      { '<Leader>rm', '<cmd>Telescope ros msg<cr>', desc = 'Open message' },
    },
    opts = {
      log_level = 'trace',
    },
  },
}

return {
  {
    'taketwo/nvim-ros',
    keys = {
      { '<Leader>re', '<cmd>Telescope ros ed<cr>', desc = 'Open file in package' },
      { '<Leader>rm', '<cmd>Telescope ros msg<cr>', desc = 'Open message' },
    },
    opts = {
      log_level = 'trace',
    },
  },
}

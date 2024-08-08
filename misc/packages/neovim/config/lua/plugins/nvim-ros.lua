return {
  {
    'taketwo/nvim-ros',
    dependencies = {
      { 'nvim-telescope/telescope.nvim' },
      { 'taketwo/vim-ros' },
    },
    keys = {
      { '<Leader>r', '', desc = 'ROS' },
      { '<Leader>re', '<cmd>Telescope ros ed<cr>', desc = 'Open file in package' },
      { '<Leader>rm', '<cmd>Telescope ros msg<cr>', desc = 'Open message' },
      { '<Leader>rs', '<cmd>Telescope ros srv<cr>', desc = 'Open service' },
      { '<Leader>ra', '<cmd>Telescope ros action<cr>', desc = 'Open action' },
    },
    ft = { 'rosmsg', 'rossrv', 'rosaction' },
    opts = {
      log_level = 'trace',
    },
  },
}

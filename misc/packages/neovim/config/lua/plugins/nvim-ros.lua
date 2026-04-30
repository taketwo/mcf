return {
  {
    'taketwo/nvim-ros',
    dependencies = {
      { 'folke/snacks.nvim' },
      { 'taketwo/vim-ros' },
    },
    keys = {
      { '<Leader>r', '', desc = 'ROS' },
      { '<Leader>rf', function() require('nvim-ros.snacks.pickers').file_picker() end, desc = 'Open file' },
      { '<Leader>rm', function() require('nvim-ros.snacks.pickers').msg_picker() end, desc = 'Open message' },
      { '<Leader>rs', function() require('nvim-ros.snacks.pickers').srv_picker() end, desc = 'Open service' },
      { '<Leader>ra', function() require('nvim-ros.snacks.pickers').action_picker() end, desc = 'Open action' },
    },
    ft = { 'rosmsg', 'rossrv', 'rosaction' },
    opts = {
      log_level = 'trace',
    },
  },
}

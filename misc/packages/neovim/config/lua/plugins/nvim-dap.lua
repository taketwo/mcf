return {
  {
    'mfussenegger/nvim-dap',
    keys = { { '<Leader>D', function() require('dap').continue() end, desc = 'Start debugging' } },
    dependencies = {
      'theHamsta/nvim-dap-virtual-text',
      'rcarriga/nvim-dap-ui',
      -- TODO: Try out
      -- "nvim-telescope/telescope-dap.nvim",
    },
    config = function() require('config.dap').setup() end,
  },
}
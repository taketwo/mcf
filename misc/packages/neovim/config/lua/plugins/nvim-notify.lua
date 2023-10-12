return {
  {
    'rcarriga/nvim-notify',
    event = 'VeryLazy',
    keys = {
      {
        '<Leader>un',
        function() require('notify').dismiss({ silent = true, pending = true }) end,
        desc = 'Dismiss notifications',
      },
    },
    opts = {
      timeout = 3000,
      max_height = function() return math.floor(vim.o.lines * 0.75) end,
      max_width = function() return math.floor(vim.o.columns * 0.75) end,
      on_open = function(win) vim.api.nvim_win_set_config(win, { zindex = 100 }) end,
      background_colour = '#000000',
    },
    config = function(_, opts)
      -- NOTE: May need to remove this if adopting Noice
      vim.notify = require('notify')
      vim.notify.setup(opts)
    end,
  },
}

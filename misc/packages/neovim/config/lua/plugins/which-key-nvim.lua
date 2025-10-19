return {
  {
    'folke/which-key.nvim',
    event = 'VeryLazy',
    keys = {
      {
        '<leader>?',
        function() require('which-key').show({ global = false }) end,
        desc = 'Buffer local keymaps',
      },
      {
        '<c-w><space>',
        function() require('which-key').show({ keys = '<c-w>', loop = true }) end,
        desc = 'Window hydra mode',
      },
    },
    opts = {
      preset = 'classic',
      keys = {
        scroll_down = '<PageDown>',
        scroll_up = '<PageUp>',
      },
      sort = {},
      icons = { mappings = false },
      spec = {
        { '<Leader>.', group = 'Search' },
        { '<Leader>a', group = 'AI support' },
        { '<Leader>f', group = 'Filename' },
        { '<Leader>g', group = 'Git' },
        { '<Leader>s', group = 'Shift/swap object' },
        { '<Leader>u', group = 'Toggle buffer options' },
        { ']', group = 'Jump next' },
        { '[', group = 'Jump previous' },
        { 'g', group = 'Go to' },
        { 'gx', group = 'Open with system application' },
        { 'z', group = 'Folds and spelling' },
        -- Same as default, but with capitalized name
        { '"', group = 'Registers' },
        { "'", group = 'Marks' },
        { '<C-w>', group = 'Window' },
      },
    },
    config = function(_, opts)
      local wk = require('which-key')
      wk.setup(opts)
    end,
  },
}

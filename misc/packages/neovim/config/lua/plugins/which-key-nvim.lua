return {
  {
    'folke/which-key.nvim',
    opts = {
      plugins = {
        presets = {
          -- Disable operators to avoid delay on pressing "c"
          -- Instead, we enable them selectively under the 'operators' key
          operators = false,
        },
      },
      operators = {
        k = 'Change', -- does not have effect
        y = 'Yank',
        d = 'Delete',
        v = 'Visual',
      },
      triggers_blacklist = {
        i = { '.' },
      },
    },
    config = function(_, opts)
      local wk = require('which-key')
      wk.setup(opts)
      wk.register({
        ['<Leader>'] = {
          ['.'] = {
            name = 'Telescope',
          },
          g = {
            name = 'Git',
          },
          l = {
            name = 'LSP',
          },
          s = {
            name = 'Shift object',
          },
        },
        [']'] = { name = 'Jump next' },
        ['['] = { name = 'Jump previous' },
      })
    end,
  },
}

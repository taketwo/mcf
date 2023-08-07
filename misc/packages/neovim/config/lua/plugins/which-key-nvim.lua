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
        i = { '.', '\\' },
      },
      defaults = {
        ['<Leader>'] = {
          ['.'] = { name = 'Telescope' },
          f = { name = 'Filename' },
          g = { name = 'Git' },
          l = { name = 'LSP' },
          s = { name = 'Shift/swap object' },
          u = { name = 'Toggle buffer options' },
        },
        [']'] = { name = 'Jump next' },
        ['['] = { name = 'Jump previous' },
        g = { name = 'Go to' },
        z = { name = 'Folds and spelling' },
        -- Same as default, but with capitalized name
        ['"'] = { name = 'Registers' },
        ['@'] = { name = 'Registers' },
        ["'"] = { name = 'Marks' },
        ['`'] = { name = 'Marks' },
        ['<C-w>'] = { name = 'Window' },
      },
    },
    config = function(_, opts)
      local wk = require('which-key')
      wk.setup(opts)
      wk.register(opts.defaults)
    end,
  },
}

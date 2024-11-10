return {
  'folke/snacks.nvim',
  priority = 1000,
  lazy = false,
  ---@type snacks.Config
  opts = {
    bigfile = { enabled = false },
    bufdelete = { enabled = false },
    debug = { enabled = false },
    git = { enabled = false },
    gitbrowse = { enabled = false },
    lazygit = { enabled = false },
    notify = { enabled = false },
    notifier = { enabled = false },
    quickfile = { enabled = false },
    rename = { enabled = true },
    statuscolumn = { enabled = false },
    terminal = {
      enabled = true,
      float = {
        keys = {
          nav_h = { '<C-h>', '<cmd>wincmd h<cr>', desc = 'Go to left window', expr = true, mode = 't' },
          nav_j = { '<C-t>', '<cmd>wincmd j<cr>', desc = 'Go to lower window', expr = true, mode = 't' },
          nav_k = { '<C-c>', '<cmd>wincmd k<cr>', desc = 'Go to upper window', expr = true, mode = 't' },
          nav_l = { '<C-n>', '<cmd>wincmd l<cr>', desc = 'Go to right window', expr = true, mode = 't' },
        },
      },
    },
    toggle = { enabled = true },
    win = { enabled = false },
    words = { enabled = false },
  },
  config = function(_, opts)
    local Snacks = require('snacks')

    -- Snacks.toggle provides a convenient way to create toggle keymaps, however the descriptions
    -- that it attaches to the keymaps are capitalized. We patch the toggle generator function to
    -- lowercase the descriptions.
    local original_toggle_new = Snacks.toggle.new
    Snacks.toggle.new = function(...)
      local toggle = original_toggle_new(...)
      if toggle.opts and toggle.opts.name then toggle.opts.name = toggle.opts.name:lower() end
      return toggle
    end

    Snacks.setup(opts)
    _G.Snacks = Snacks
  end,
}

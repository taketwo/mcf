return {
  'folke/snacks.nvim',
  priority = 1000,
  lazy = false,
  ---@type snacks.Config
  opts = {
    bigfile = { enabled = true },
    bufdelete = { enabled = false }, -- Barbar does a good job deleting buffers and preserving window layout
    debug = { enabled = true },
    git = { enabled = true },
    gitbrowse = { enabled = false }, -- TODO: Consider enabling
    lazygit = { enabled = true }, -- TODO: Evaluating this
    notify = { enabled = true },
    notifier = { enabled = true },
    quickfile = { enabled = true },
    rename = { enabled = true },
    statuscolumn = { enabled = false }, -- Waste of space
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
    win = { enabled = true },
    words = { enabled = false }, -- Illuminate functionality and UX are more appealing
  },
  keys = {
    { '<Leader>gB', function() Snacks.git.blame_line() end, desc = 'Blame current line' },
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

    -- Snacks.notifier replaced nvim-notify, however it does not provide a convenient command to
    -- display notifications history.
    vim.cmd('command! -nargs=0 Notifications lua Snacks.notifier.show_history()')

    Snacks.setup(opts)
    _G.Snacks = Snacks
  end,
}

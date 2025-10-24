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
    gitbrowse = { enabled = false }, --  Gitlinker is more advanced
    image = { enabled = false }, -- Not supported in Alacritty
    indent = {
      enabled = true,
      animate = { enabled = false },
      scope = { enabled = false }, -- TODO: Consider using this instead of mini.indentscope
    },
    lazygit = { enabled = true },
    notify = { enabled = true },
    notifier = { enabled = true },
    picker = {
      layout = {
        preset = function() return vim.o.columns >= 120 and 'telescope' or 'vertical' end,
      },
      actions = {
        sidekick_send = function(...) return require('sidekick.cli.picker.snacks').send(...) end,
      },
      win = {
        input = {
          keys = {
            ['<A-a>'] = { 'sidekick_send', mode = { 'n', 'i' } },
            ['<A-c>'] = { 'list_up', mode = { 'n', 'i' } },
            ['<A-t>'] = { 'list_down', mode = { 'n', 'i' } },
            ['<C-h>'] = { 'preview_scroll_up', mode = { 'n', 'i' } },
            ['<C-n>'] = { 'preview_scroll_down', mode = { 'n', 'i' } },
            ['<Esc>'] = { 'close', mode = { 'n', 'i' } },
            ['<PageDown>'] = { 'list_scroll_down', mode = { 'n', 'i' } },
            ['<PageUp>'] = { 'list_scroll_up', mode = { 'n', 'i' } },
          },
        },
      },
    },
    quickfile = { enabled = true },
    rename = { enabled = true },
    scope = { enabled = false }, -- TODO: Consider using this instead of mini.indentscope
    statuscolumn = { enabled = false }, -- Waste of horizontal space
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
    { '<Leader>gy', function() Snacks.lazygit.open() end, desc = 'Open Lazygit' },
    -- Picker: Search
    { '<Leader>."', function() Snacks.picker.registers() end, desc = 'Registers' },
    { '<Leader>.,', function() Snacks.picker.resume() end, desc = 'Resume last picker' },
    { '<Leader>..', function() Snacks.picker.files() end, desc = 'Files' },
    { '<Leader>./', function() Snacks.picker.search_history() end, desc = 'Search history' },
    { '<Leader>.;', function() Snacks.picker.command_history() end, desc = 'Command history' },
    { '<Leader>.D', function() Snacks.picker.diagnostics_buffer() end, desc = 'Diagnostics (buffer only)' },
    { '<Leader>.H', function() Snacks.picker.highlights() end, desc = 'Highlights' },
    { '<Leader>.a', function() Snacks.picker.autocmds() end, desc = 'Autocmds' },
    { '<Leader>.b', function() Snacks.picker.buffers() end, desc = 'Buffers' },
    { '<Leader>.c', function() Snacks.picker.commands() end, desc = 'Commands' },
    { '<Leader>.d', function() Snacks.picker.diagnostics() end, desc = 'Diagnostics' },
    { '<Leader>.f', function() Snacks.picker.files() end, desc = 'Files' },
    { '<Leader>.g', function() Snacks.picker.grep() end, desc = 'Grep' },
    { '<Leader>.h', function() Snacks.picker.help() end, desc = 'Help pages' },
    { '<Leader>.i', function() Snacks.picker.icons() end, desc = 'Icons' },
    { '<Leader>.k', function() Snacks.picker.keymaps() end, desc = 'Keymaps' },
    { '<Leader>.l', function() Snacks.picker.loclist() end, desc = 'Location list' },
    { '<Leader>.m', function() Snacks.picker.marks() end, desc = 'Marks' },
    { '<Leader>.q', function() Snacks.picker.qflist() end, desc = 'Quickfix list' },
    { '<Leader>.r', function() Snacks.picker.recent() end, desc = 'Recent files' },
    { '<Leader>.s', function() Snacks.picker.smart() end, desc = 'Smart files' },
    { '<Leader>.u', function() Snacks.picker.undo() end, desc = 'Undo history' },
    { '<Leader>.w', function() Snacks.picker.grep_word() end, desc = 'Visual selection or word', mode = { 'n', 'x' } },
    -- Picker: Git
    { '<Leader>gC', function() Snacks.picker.git_branches() end, desc = 'Checkout branch' },
    { '<Leader>gL', function() Snacks.picker.git_log_file() end, desc = 'Log (current file)' },
    { '<Leader>gl', function() Snacks.picker.git_log() end, desc = 'Log' },
    -- Picker: Misc
    { 'z=', function() Snacks.picker.spelling() end, desc = 'Spelling suggestions' },
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
    vim.cmd('command! -nargs=0 Notifications lua Snacks.notifier.show_history({reverse = true})')

    Snacks.setup(opts)
    _G.Snacks = Snacks
  end,
}

local available, wk = pcall(require, 'which-key')
if not available then return end

wk.register({
  [';'] = { ':', 'Enter command mode', silent = false },

  -- Move around in normal and visual (but not select) modes
  -- The up and down motions take into account line wrapping
  --
  --       c
  --     h t n
  --
  h = { 'h', 'Left', mode = { 'n', 'x' } },
  n = { 'l', 'Right', mode = { 'n', 'x' } },
  c = { 'v:count == 0 ? "gk" : "k"', 'Up', mode = { 'n', 'x' }, expr = true },
  t = { 'v:count == 0 ? "gj" : "j"', 'Down', mode = { 'n', 'x' }, expr = true },
  -- Accelerated up and down (a.k.a. scrolling)
  C = { '<PageUp>', 'Scroll window up' },
  T = { '<PageDown>', 'Scroll window down' },
  -- Beginning and end of line
  ['-'] = { '$', 'End of line', mode = { 'n', 'v' } },
  ['_'] = { '^', 'Start of line (non-blank)', mode = { 'n', 'v' } },

  -- Change (delete-insert) mode
  k = { 'c', 'Delete and enter insert mode', mode = { 'n', 'x' } },
  kk = { 'cc', 'Delete line and enter insert mode' },
  kK = { 'C', 'Delete until end of line and enter insert mode' },

  -- Search
  -- Seek through search results with l and L, centering the screen after jumps
  l = { 'nzz', 'Next search result' },
  L = { 'Nzz', 'Previous search result' },
  -- Seek through search results with Ctrl+c and Ctrl+t whilst entering search pattern
  ['<C-c>'] = { '<C-t>', 'Next search result', mode = 'c', silent = false },
  ['<C-t>'] = { '<C-g>', 'Previous search result', mode = 'c', silent = false },
  ['<Esc>'] = { ':nohlsearch<Bar>:echo<CR>', 'Clear search highlight' },

  -- Window management
  -- Move between windows without releasing Ctrl
  -- Additional mappings with Alt prefix are created in vim-tmux-navigator plugin
  ['<C-w><C-h>'] = { '<C-w>h', 'Go to left window' },
  ['<C-w><C-n>'] = { '<C-w>l', 'Go to right window' },
  ['<C-w><C-c>'] = { '<C-w>k', 'Go to upper window' },
  ['<C-w><C-t>'] = { '<C-w>j', 'Go to lower window' },
  -- Resize windows with Alt+arrow
  ['<M-Up>'] = { '<C-w>+', 'Increase window height' },
  ['<M-Down>'] = { '<C-w>-', 'Decrease window height' },
  ['<M-Left>'] = { '<C-w><', 'Decrease window width' },
  ['<M-Right>'] = { '<C-w>>', 'Increase window width' },

  -- Line operations
  ['<cr>'] = { '<cmd>call append(line("."), "")<cr>', 'Insert new line below' },
  ['<C-j>'] = { 'J', 'Join lines' }, -- because J is used by 'leap.nvim'
  -- TODO: Consider using a different keymap for line duplication
  ['<Leader>d'] = { '<cmd>t.<cr>', 'Duplicate current line' },
  Y = { 'y$', 'Yank to end of line' },

  -- Filename operations
  ['<Leader>f'] = {
    name = 'Filename',
    s = { '<cmd>let @+=expand("%")<cr>', 'Copy filename to clipboard' },
    l = { '<cmd>let @+=expand("%:p")<cr>', 'Copy file path to clipboard' },
  },

  -- Misc
  -- TODO: This needs to be mapped to auto-pairs plugin
  ['<C-d>'] = { '<BS>', 'Delete last entered character', mode = 'i' },
  ['<F7>'] = { '<cmd>setlocal spell!<cr>', 'Enable spellcheck' },
  ['<F9>'] = { '<cmd>MakeTarget<cr>', 'Run make', mode = { 'n', 'i' } },

  -- NOTE: Some keys that are still free: $ ^ F5 F10
})

-- Move lines
-- Use separate register() calls because while the keys and the descriptions are the same for
-- different modes, the implementations are different
wk.register({
  ['<C-c>'] = { '<cmd>m .-2<cr>', 'Move line up' },
  ['<C-t>'] = { '<cmd>m .+1<cr>', 'Move line down' },
  ['<C-h>'] = { '<cmd><<cr>', 'Decrease line indentation' },
  ['<C-n>'] = { '<cmd>><cr>', 'Increase line indentation' },
}, { mode = 'n' })
wk.register({
  ['<C-c>'] = { '<Esc>:m .-2<cr>==gi', 'Move line up' },
  ['<C-t>'] = { '<Esc>:m .+1<cr>==gi', 'Move line down' },
  ['<C-h>'] = { '<C-d>', 'Decrease line indentation' },
  ['<C-n>'] = { '<C-t>', 'Increase line indentation' },
}, { mode = 'i' })
wk.register({
  ['<C-c>'] = { ":m '<-2<cr>gv", 'Move lines up' },
  ['<C-t>'] = { ":m '>+1<cr>gv", 'Move lines down' },
  ['<C-h>'] = { ':<<cr>gv', 'Decrease lines indentation' },
  ['<C-n>'] = { ':><cr>gv', 'Increase lines indentation' },
}, { mode = 'v' })
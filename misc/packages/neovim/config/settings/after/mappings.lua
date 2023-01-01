require('which-key').register({
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

  -- Search
  -- Seek through search results with l and L, centering the screen after jumps
  l = { 'nzz', 'Next search result' },
  L = { 'Nzz', 'Previous search result' },
  -- Seek through search results with Ctrl+c and Ctrl+t whilst entering search pattern
  ['<C-c>'] = { '<C-t>', 'Next search result', mode = 'c', silent = false },
  ['<C-t>'] = { '<C-g>', 'Previous search result', mode = 'c', silent = false },
  ['<Esc>'] = { ':nohlsearch<Bar>:echo<CR>', 'Clear search highlight' },
})

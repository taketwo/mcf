local Util = require('utils')

local function map(mode, lhs, rhs, opts)
  opts = opts or {}
  opts.silent = opts.silent == nil and true or opts.silent
  vim.keymap.set(mode, lhs, rhs, opts)
end

map('n', ';', ':', { desc = 'Enter command mode', silent = false })

-- Move around in normal and visual (but not select) modes
-- The up and down motions take into account line wrapping
--
--       c
--     h t n
--
map({ 'n', 'x' }, 'h', 'h', { desc = 'Left' })
map({ 'n', 'x' }, 'n', 'l', { desc = 'Right' })
map({ 'n', 'x' }, 'c', "v:count == 0 ? 'gk' : 'k'", { desc = 'Up', expr = true })
map({ 'n', 'x' }, 't', "v:count == 0 ? 'gj' : 'j'", { desc = 'Down', expr = true })
-- -- Accelerated up and down (a.k.a. scrolling)
map('n', 'C', '<PageUp>', { desc = 'Scroll window up' })
map('n', 'T', '<PageDown>', { desc = 'Scroll window down' })
-- -- Beginning and end of line
map({ 'n', 'v' }, '-', '$', { desc = 'End of line' })
map({ 'n', 'v' }, '_', '^', { desc = 'Start of line (non-blank)' })

-- Change (delete-insert) mode
map({ 'n', 'x' }, 'k', 'c', { desc = 'Delete and enter insert mode' })
map('n', 'kk', 'cc', { desc = 'Delete line and enter insert mode' })
map('n', 'kK', 'C', { desc = 'Delete until end of line and enter insert mode' })

-- Search
-- Seek through search results with l and L, centering the screen after jumps
map('n', 'l', 'nzz', { desc = 'Next search result' })
map('n', 'L', 'Nzz', { desc = 'Previous search result' })
-- Seek through search results with Ctrl+c and Ctrl+t whilst entering search pattern
map('c', '<C-c>', '<C-t>', { desc = 'Next search result', silent = false })
map('c', '<C-t>', '<C-g>', { desc = 'Previous search result', silent = false })
-- Clear search highlight
map('n', '<Esc>', ':nohlsearch<Bar>:echo<CR>', { desc = 'Clear search highlight' })

-- Window management
-- Move between windows without releasing Ctrl
-- Additional mappings with Alt prefix are created in vim-tmux-navigator plugin
map('n', '<C-w><C-h>', '<C-w>h', { desc = 'Go to left window' })
map('n', '<C-w><C-n>', '<C-w>l', { desc = 'Go to right window' })
map('n', '<C-w><C-c>', '<C-w>k', { desc = 'Go to upper window' })
map('n', '<C-w><C-t>', '<C-w>j', { desc = 'Go to lower window' })
-- Resize windows with Alt+arrow
map('n', '<M-Up>', '<C-w>+', { desc = 'Increase window height' })
map('n', '<M-Down>', '<C-w>-', { desc = 'Decrease window height' })
map('n', '<M-Left>', '<C-w><', { desc = 'Decrease window width' })
map('n', '<M-Right>', '<C-w>>', { desc = 'Increase window width' })

-- Line operations
map('n', '<cr>', '<cmd>call append(line("."), "")<cr>', { desc = 'Insert new line below' })
map('n', '<C-j>', 'J', { desc = 'Join lines' }) -- because J is used by 'leap.nvim'
map('n', '<C-d>', '<cmd>t.<cr>', { desc = 'Duplicate current line' })
map('n', 'Y', 'y$', { desc = 'Yank to end of line' })

-- Move lines
map('n', '<C-c>', '<cmd>m .-2<cr>', { desc = 'Move line up' })
map('n', '<C-t>', '<cmd>m .+1<cr>', { desc = 'Move line down' })
map('n', '<C-h>', '<cmd><<cr>', { desc = 'Decrease line indentation' })
map('n', '<C-n>', '<cmd>><cr>', { desc = 'Increase line indentation' })
map('i', '<C-c>', '<Esc>:m .-2<cr>==gi', { desc = 'Move line up' })
map('i', '<C-t>', '<Esc>:m .+1<cr>==gi', { desc = 'Move line down' })
map('i', '<C-h>', '<C-d>', { desc = 'Decrease line indentation' })
map('i', '<C-n>', '<C-t>', { desc = 'Increase line indentation' })
map('v', '<C-c>', ":m '<-2<cr>gv", { desc = 'Move lines up' })
map('v', '<C-t>', ":m '>+1<cr>gv", { desc = 'Move lines down' })
map('v', '<C-h>', ':<<cr>gv', { desc = 'Decrease lines indentation' })
map('v', '<C-n>', ':><cr>gv', { desc = 'Increase lines indentation' })

-- Filename operations
map('n', '<Leader>fs', '<cmd>let @+=expand("%")<cr>', { desc = 'Copy filename to clipboard' })
map('n', '<Leader>fl', '<cmd>let @+=expand("%:p")<cr>', { desc = 'Copy file path to clipboard' })
map('n', '<Leader>fp', '<C-g>', { desc = 'Print file path and status' })

-- Add undo breakpoints
map('i', ',', ',<c-g>u', { desc = 'Insert , and add undo breakpoint' })
map('i', '.', '.<c-g>u', { desc = 'Insert . and add undo breakpoint' })
map('i', ';', ';<c-g>u', { desc = 'Insert ; and add undo breakpoint' })

-- Toggle buffer options
map('n', '<Leader>us', function() Util.toggle('spell') end, { desc = 'Toggle spellchecking' })
map('n', '<Leader>uw', function() Util.toggle('wrap') end, { desc = 'Toggle word wraping' })
map('n', '<Leader>ul', function()
  Util.toggle('relativenumber', true)
  Util.toggle('number')
end, { desc = 'Toggle line numbers' })
local conceallevel = vim.o.conceallevel > 0 and vim.o.conceallevel or 3
map(
  'n',
  '<Leader>uc',
  function() Util.toggle('conceallevel', false, { 0, conceallevel }) end,
  { desc = 'Toggle concealing' }
)
map('n', '<Leader>ud', Util.toggle_diagnostics, { desc = 'Toggle diagnostics' })
map('n', '<Leader>uf', require('config.lsp.format').toggle, { desc = 'Toggle format on save' })
map(
  'n',
  '<Leader>uF',
  function() require('config.lsp.format').toggle({ global = true }) end,
  { desc = 'Toggle format on save (globally)' }
)

-- Misc
-- TODO: This needs to be mapped to auto-pairs plugin
map('i', '<C-d>', '<BS>', { desc = 'Delete last entered character' })
map({ 'n', 'i' }, '<F9>', '<cmd>MakeTarget<cr>', { desc = 'Run make', silent = false })

-- NOTE: Some keys that are still free: $ ^ F5 F10

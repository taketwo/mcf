vim.g.mapleader = ','
vim.g.maplocalleader = '\\'

local opt = vim.opt

opt.autowrite = true -- Automatically save before e.g. :make
opt.clipboard = 'unnamedplus' -- Sync with system clipboard
opt.completeopt = 'menu,menuone,noselect'                                            -- TODO: Is menu needed?
opt.conceallevel = 3 -- Hide * markup for bold and italic                            -- TODO: Good idea?
opt.confirm = true -- Confirm to save changes before exiting modified buffer         -- TODO: Good idea?
opt.cursorline = true -- Enable highlighting of the current line
opt.cursorlineopt = 'number' -- Only make current line number bold
opt.expandtab = true -- Use spaces instead of tabs
opt.foldenable = false -- Disable folding by default
opt.foldmethod = 'indent' -- Fold based on indent
opt.foldnestmax = 3 -- Maximum number of nested folds
opt.formatoptions = 'tcro/qnljp'
opt.grepformat = '%f:%l:%c:%m'
opt.grepprg = 'rg --vimgrep'
opt.guicursor = 'a:blinkon0' -- Disable blinking cursor
opt.ignorecase = true -- Ignore case when searching
opt.inccommand = 'nosplit' -- Preview incremental substitute
opt.laststatus = 2 -- Always show statusline                                         -- TODO: Consider trying 3
opt.list = true -- Show some invisible characters
opt.listchars = { tab = '→ ', trail = '·' } -- Show tabs and trailing spaces
opt.matchpairs:append({ '<:>' }) -- Match angle brackets
opt.mouse = 'a' -- Enable mouse in all modes                                         -- TODO: Good idea?
opt.number = true -- Print line number
opt.pumheight = 15 -- Maximum number of entries in a popup
opt.relativenumber = true -- Relative line numbers
opt.scrolloff = 8 -- Start scrolling when we are 8 lines away from margins
opt.shiftround = true -- Round indent to multiple of 'shiftwidth'
opt.shada = {
  '!', -- Save and restore global variables
  "'1000", -- Maximum number of previously edited files to remember marks for
  '<50', -- Maximum number of lines in register
  'f0', -- No file marks                                                              FIXME: This has no effect
  's10', -- Maximum size of item contents (KiB)
  'h', -- Disable effect of hlsearch when loading
}
opt.shiftwidth = 2 -- Size of an indent
opt.shortmess:append({ C = true }) -- Do not give messages while scanning for ins-completion items
opt.shortmess:append({ I = true }) -- Do not give intro message when srating Neovim
opt.shortmess:append({ W = true }) -- Do not give "written" messages
opt.shortmess:append({ c = true }) -- Do not give ins-completion-menu messages
opt.shortmess:append({ s = true }) -- Do not give "search hit BOTTOM" messages
opt.showmode = false -- Dont show mode since we have a statusline
opt.sidescrolloff = 15 -- Columns of context
opt.smartcase = true -- Do not ignore case when query has capitals
opt.smartindent = true -- Insert indents automatically
opt.softtabstop = 2 -- Number of spaces tabs count for when editing                  -- TODO: Still needed?
opt.spelllang = { 'en_us' } -- Spell checking language
opt.splitbelow = true -- New splits, help, scratch preview all go to the bottom
opt.splitkeep = 'screen' -- Keep text on the same screen line in horizontal split    -- TODO: Good idea?
opt.swapfile = false -- Disable swap file
opt.tabstop = 2 -- Number of spaces tabs count for
opt.termguicolors = true -- True color support
opt.timeoutlen = 300 -- Time to wait for a mapped sequence to complete               -- TODO: Good idea?
opt.undofile = true -- Persistent undo
opt.updatetime = 200 -- Trigger CursorHold after inactivity of this length           -- TODO: Good idea?
opt.visualbell = true -- No sounds
opt.wildignore = { '*.o', '*.obj', '*.so', '*.pyc', '*.pyo', '*.swp' } -- Files to ignore when completing
opt.wildmode = 'longest:full,full' -- Command-line completion until longest match, then iterate over full matches
opt.winminwidth = 5 -- Minimum window width

local treesitter_configs = require('nvim-treesitter.configs')

treesitter_configs.setup({
  ensure_installed = { -- one of "all", "maintained", or a list of languages
    'bash',
    'c',
    'cmake',
    'comment',
    'cpp',
    'diff',
    'gitcommit',
    'haskell',
    'hcl', -- for Terraform files
    'json',
    'make',
    -- "markdown",    seems to be broken
    'nix',
    'python',
    'regex',
    'toml',
    'vim',
    'yaml',
  },
  highlight = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = '=',
      node_incremental = '=',
      scope_incremental = '/',
      node_decremental = '+',
    },
  },
  indent = {
    enable = true,
    disable = { "cpp", "python" }, -- currently misbehaving
  },
  playground = {
    enable = true,
  },
  textobjects = {
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ['af'] = { query = '@function.outer', desc = 'function' },
        ['if'] = { query = '@function.inner', desc = 'function' },
        ['ac'] = { query = '@class.outer', desc = 'class' },
        ['ic'] = { query = '@class.inner', desc = 'class' },
        ['al'] = { query = '@loop.outer', desc = 'loop' },
        ['il'] = { query = '@loop.inner', desc = 'loop' },
        ['as'] = { query = '@statement.outer', desc = 'statement' },
        ['is'] = { query = '@statement.inner', desc = 'statement' },
      },
      selection_modes = {
        ['@function.outer'] = 'V', -- FIXME: Does not seem to work
      },
    },
    swap = {
      enable = true,
      swap_next = {
        ['<leader>sn'] = { query = '@parameter.inner', desc = 'Shift parameter right' },
        ['<leader>st'] = { query = '@function.outer', desc = 'Shift function down' },
        ['<leader>sT'] = { query = '@class.outer', desc = 'Shift class down' },
      },
      swap_previous = {
        ['<leader>sh'] = { query = '@parameter.inner', desc = 'Shift parameter left' },
        ['<leader>sc'] = { query = '@function.outer', desc = 'Shift function up' },
        ['<leader>sC'] = { query = '@class.outer', desc = 'Shift class up' },
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        [']m'] = { query = '@function.outer', desc = 'Next method start' },
        [']]'] = { query = '@class.outer', desc = 'Next class start' },
        [']p'] = { query = '@parameter.inner', desc = 'Next parameter start' },
      },
      goto_next_end = {
        [']M'] = { query = '@function.outer', desc = 'Next method end' },
        [']['] = { query = '@class.outer', desc = 'Next class end' },
      },
      goto_previous_start = {
        ['[m'] = { query = '@function.outer', desc = 'Previous method start' },
        ['[['] = { query = '@class.outer', desc = 'Previous class start' },
        ['[p'] = { query = '@parameter.inner', desc = 'Previous parameter start' },
      },
      goto_previous_end = {
        ['[M'] = { query = '@function.outer', desc = 'Previous method end' },
        ['[]'] = { query = '@class.outer', desc = 'Previous class end' },
      },
    },
  },
})

-- Set names for prefix groups
require('which-key').register({
  ['<Leader>s'] = { name = "Shift object" },
  [']'] = { name = "Jump next" },
  ['['] = { name = "Jump previous" }
})

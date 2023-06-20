return {
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      { 'nvim-treesitter/nvim-treesitter-textobjects' },
      { 'nvim-treesitter/nvim-treesitter-context' },
    },
    build = ':TSUpdate',
    event = { 'BufReadPost', 'BufNewFile' },
    cmd = { 'TSUpdateSync' },
    opts = {
      ensure_installed = {
        'bash',
        'c',
        'cmake',
        'comment',
        'cpp',
        'diff',
        'gitcommit',
        'glsl',
        'haskell',
        'hcl', -- for Terraform files
        -- 'hjson', seems to be broken, using a plugin instead
        'json',
        'jsonc',
        'lua',
        'luadoc',
        'luap',
        'make',
        'markdown',
        'markdown_inline',
        'nix',
        'python',
        'regex',
        'toml',
        'vim',
        'vimdoc',
        'yaml',
      },
      highlight = { enable = true },
      indent = {
        enable = true,
        disable = { 'cpp' }, -- currently misbehaving
      },
      playground = {
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
      textobjects = {
        select = { enable = false },
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
            [']c'] = { query = '@class.outer', desc = 'Next class start' },
            [']p'] = { query = '@parameter.inner', desc = 'Next parameter start' },
          },
          goto_next_end = {
            [']M'] = { query = '@function.outer', desc = 'Next method end' },
            [']C'] = { query = '@class.outer', desc = 'Next class end' },
          },
          goto_previous_start = {
            ['[m'] = { query = '@function.outer', desc = 'Previous method start' },
            ['[c'] = { query = '@class.outer', desc = 'Previous class start' },
            ['[p'] = { query = '@parameter.inner', desc = 'Previous parameter start' },
          },
          goto_previous_end = {
            ['[M'] = { query = '@function.outer', desc = 'Previous method end' },
            ['[C'] = { query = '@class.outer', desc = 'Previous class end' },
          },
        },
      },
    },
    config = function(_, opts) require('nvim-treesitter.configs').setup(opts) end,
  },
}

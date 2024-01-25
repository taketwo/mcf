return {
  {
    'nvim-treesitter/nvim-treesitter',
    dependencies = {
      { 'nvim-treesitter/nvim-treesitter-textobjects' },
      { 'nvim-treesitter/nvim-treesitter-context', opts = { max_lines = 3 } },
    },
    build = ':TSUpdate',
    event = { 'BufReadPost', 'BufNewFile' },
    cmd = { 'TSUpdateSync' },
    init = function(plugin)
      -- NOTE: This hack is taken from LazyVim and should be kept in sync.
      -- PERF: Add nvim-treesitter queries to the rtp and it's custom query predicates early
      -- This is needed because a bunch of plugins no longer `require("nvim-treesitter")`, which
      -- no longer trigger the **nvim-treeitter** module to be loaded in time.
      -- Luckily, the only thins that those plugins need are the custom queries, which we make available
      -- during startup.
      require('lazy.core.loader').add_to_rtp(plugin)
      require('nvim-treesitter.query_predicates')
    end,
    opts = {
      -- Some of the following parsers are installed by default, but we want to ensure that our config is
      -- not affected by future changes to the default parsers.
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
        'xml',
        'yaml',
      },
      highlight = { enable = true },
      indent = {
        enable = true,
        disable = { 'cpp' }, -- currently misbehaving
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

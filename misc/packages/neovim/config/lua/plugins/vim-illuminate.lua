return {
  {
    'RRethy/vim-illuminate',
    event = { 'BufReadPost', 'BufNewFile' },
    keys = {
      { ']]', function() require('illuminate').goto_next_reference() end, desc = 'Go to next reference' },
      { '[[', function() require('illuminate').goto_prev_reference() end, desc = 'Go to previous reference' },
      -- TODO: The mapping below conflicts with leap-spooky. Come up with a different mapping/mnemonic.
      -- { 'ir', function() require('illuminate').textobj_select() end, mode = { 'o', 'x' }, desc = 'reference' },
    },
    opts = {
      delay = 200,
      large_file_cutoff = 2000,
      large_file_overrides = {
        providers = { 'lsp' },
      },
      filetypes_denylist = {
        'magit',
        'markdown',
      },
    },
    config = function(_, opts) require('illuminate').configure(opts) end,
  },
}

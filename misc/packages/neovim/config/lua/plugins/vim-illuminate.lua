return {
  {
    'RRethy/vim-illuminate',
    event = { 'BufReadPost', 'BufNewFile' },
    keys = {
      { ']]', function() require('illuminate').goto_next_reference(false) end, desc = 'Go to next reference' },
      { '[[', function() require('illuminate').goto_prev_reference(false) end, desc = 'Go to previous reference' },
      -- TODO: The mapping below conflicts with leap-spooky. Come up with a different mapping/mnemonic.
      -- { 'ir', function() require('illuminate').textobj_select() end, mode = { 'o', 'x' }, desc = 'reference' },
    },
    opts = {
      delay = 200,
      filetypes_denylist = {
        'magit',
        'markdown',
      },
    },
    config = function(_, opts) require('illuminate').configure(opts) end,
  },
}

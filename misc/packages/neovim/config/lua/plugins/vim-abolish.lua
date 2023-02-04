return {
  {
    'tpope/vim-abolish',
    init = function() vim.g.abolish_no_mappings = 1 end,
    config = function()
      require('which-key').register({
        ['kc'] = { '<Plug>(abolish-coerce-word)', 'Change case' },
      })
    end,
  },
}

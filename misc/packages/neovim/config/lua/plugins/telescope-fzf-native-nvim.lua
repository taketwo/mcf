return {
  {
    'nvim-telescope/telescope-fzf-native.nvim',
    build = 'make',
    lazy = true,
    config = function() require('telescope').load_extension('fzf') end,
  },
}

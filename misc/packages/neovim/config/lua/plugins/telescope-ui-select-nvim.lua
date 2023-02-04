return {
  {
    'nvim-telescope/telescope-ui-select.nvim',
    lazy = true,
    config = function() require('telescope').load_extension('ui-select') end,
  },
}

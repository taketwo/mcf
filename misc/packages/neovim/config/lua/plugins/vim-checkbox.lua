return {
  {
    'jkramer/vim-checkbox',
    ft = { 'markdown' },
    init = function() vim.g.checkbox_states = { ' ', '+' } end,
  },
}

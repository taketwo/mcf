return {
  {
    'zhimsel/vim-checkbox',
    branch = 'change-map',
    ft = { 'markdown' },
    init = function()
      vim.g.checkbox_states = { ' ', '+' }
      vim.g.checkbox_create_maps = 0
    end,
  },
}

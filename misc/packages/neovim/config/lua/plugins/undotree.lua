return {
  {
    'mbbill/undotree',
    cmd = { 'UndotreeToggle' },
    keys = {
      { '<F4>', '<cmd>UndotreeToggle<cr>', desc = 'Toggle undo tree' },
    },
    init = function() vim.g.undotree_SetFocusWhenToggle = 1 end,
  },
}

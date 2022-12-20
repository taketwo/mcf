require('which-key').register({
  ['<F4>'] = { '<cmd>UndotreeToggle<cr>', 'Toggle undo tree' },
})

vim.g.undotree_SetFocusWhenToggle = 1

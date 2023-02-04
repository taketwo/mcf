return {
  {
    'tpope/vim-dispatch',
    cmd = {
      'Copen',
      'Dispatch',
      'Make',
      'Spawn',
      'Start',
    },
    init = function() vim.g.dispatch_no_maps = 1 end,
  },
}

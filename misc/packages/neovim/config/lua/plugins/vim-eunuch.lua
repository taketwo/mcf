return {
  {
    'tpope/vim-eunuch',
    cmd = {
      'Chmod',
      'Copy',
      'Delete',
      'Duplicate',
      'Mkdir',
      'Move',
      'Remove',
      'Rename',
      'SudoEdit',
      'SudoWrite',
    },
    init = function() vim.g.eunuch_no_maps = 1 end,
  },
}

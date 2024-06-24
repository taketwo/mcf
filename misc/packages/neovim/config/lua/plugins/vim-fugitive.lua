return {
  {
    'tpope/vim-fugitive',
    cmd = { 'Git', 'Gedit' },
    keys = {
      { '<Leader>g', '', desc = 'Git' },
      { '<Leader>gr', '<cmd>Git restore %<cr>', desc = 'Reset buffer' },
    },
  },
}

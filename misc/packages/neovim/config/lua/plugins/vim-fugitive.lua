return {
  {
    'tpope/vim-fugitive',
    cmd = { 'Git', 'Gedit', 'Gdiffsplit', 'Gvdiffsplit', 'Ghdiffsplit' },
    keys = {
      { '<Leader>g', '', desc = 'Git' },
      { '<Leader>gr', '<cmd>Git restore %<cr>', desc = 'Reset buffer' },
    },
  },
}

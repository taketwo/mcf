return {
  {
    'tpope/vim-fugitive',
    cmd = { 'Git', 'Gedit' },
    keys = {
      { '<Leader>gb', '<cmd>Git blame<cr>', desc = 'Blame' },
      { '<Leader>gr', '<cmd>Git restore %<cr>', desc = 'Reset buffer' },
    },
  },
}

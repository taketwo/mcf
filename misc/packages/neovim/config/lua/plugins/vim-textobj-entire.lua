return {
  {
    'kana/vim-textobj-entire',
    dependencies = { 'kana/vim-textobj-user' },
    keys = {
      { 'ie', mode = { 'o', 'x' }, desc = 'Entire buffer excluding empty lines' },
      { 'ae', mode = { 'o', 'x' }, desc = 'Entire buffer' },
    },
  },
}

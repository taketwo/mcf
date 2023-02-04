return {
  {
    'kana/vim-textobj-entire',
    dependencies = { 'kana/vim-textobj-user' },
    keys = {
      { 'ie', mode = { 'o', 'x' }, desc = 'entire buffer (without empty lines)' },
      { 'ae', mode = { 'o', 'x' }, desc = 'entire buffer' },
    },
  },
}

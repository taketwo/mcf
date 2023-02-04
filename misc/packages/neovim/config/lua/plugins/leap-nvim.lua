return {
  {
    'ggandor/leap.nvim',
    keys = {
      { 'j', '<Plug>(leap-forward-to)', mode = { 'n', 'x', 'o' }, desc = 'Leap forward' },
      { 'J', '<Plug>(leap-backward-to)', mode = { 'n', 'x', 'o' }, desc = 'Leap backward' },
      { 'gj', '<Plug>(leap-from-window)', mode = { 'n', 'x', 'o' }, desc = 'Leap to another window' },
    },
  },
}

return {
  {
    'monaqa/dial.nvim',
    keys = {
      { '<C-q>', '<Plug>(dial-increment)', mode = { 'n', 'v' }, desc = 'Increment under cursor' },
      { '<C-x>', '<Plug>(dial-decrement)', mode = { 'n', 'v' }, desc = 'Decrement under cursor' },
    },
  },
}

return {
  {
    'numToStr/Comment.nvim',
    event = 'BufWinEnter',
    config = function()
      require('Comment').setup()
      require('Comment.ft').hjson = '#%s'
    end,
  },
}

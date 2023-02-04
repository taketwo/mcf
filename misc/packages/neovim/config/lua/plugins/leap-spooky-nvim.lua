return {
  {
    'ggandor/leap-spooky.nvim',
    event = 'BufWinEnter',
    dependencies = { 'ggandor/leap.nvim' },
    opts = {
      paste_on_remote_yank = true,
    },
  },
}

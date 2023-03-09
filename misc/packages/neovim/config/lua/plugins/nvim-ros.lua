return {
  {
    'taketwo/nvim-ros',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      log_level = 'trace',
    },
  },
}

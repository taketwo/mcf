return {
  {
    'rhysd/git-messenger.vim',
    cmd = { 'GitMessenger' },
    keys = {
      { '<Leader>gm', '<cmd>GitMessenger<cr>', 'View commit message' },
    },
    init = function() vim.g.git_messenger_no_default_mappings = 1 end,
  },
}

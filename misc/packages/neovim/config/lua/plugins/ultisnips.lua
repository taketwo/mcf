return {
  {
    'SirVer/ultisnips',
    dependencies = { 'honza/vim-snippets' },
    event = 'InsertEnter',
    init = function()
      vim.g.UltiSnipsExpandTrigger = '<C-b>'
      vim.g.UltiSnipsListSnippets = '<Leader>u'
      vim.g.UltiSnipsJumpForwardTrigger = '<C-b>'
      vim.g.UltiSnipsJumpBackwardTrigger = ''
      vim.g.UltiSnipsEditSplit = 'vertical'
      vim.g.UltiSnipsSnippetDirectories = { 'UltiSnips', 'snippet' }
      vim.g.UltiSnipsSnippetStorageDirectoryForUltiSnipsEdit = vim.fn.stdpath('config') .. '/snippet'
    end,
  },
}

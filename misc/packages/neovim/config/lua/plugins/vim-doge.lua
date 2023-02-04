return {
  {
    'kkoomen/vim-doge',
    cmd = { 'DogeGenerate' },
    keys = { '<Leader>D' },
    init = function()
      vim.g.doge_mapping = '<Leader>D'
      vim.g.doge_doc_standard_python = 'numpy'
    end,
  },
}

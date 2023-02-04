return {
  {
    'embear/vim-localvimrc',
    init = function()
      vim.g.localvimrc_name = '.vim'
      vim.g.localvimrc_count = -1
      vim.g.localvimrc_sandbox = 0
      vim.g.localvimrc_ask = 0
    end,
  },
}

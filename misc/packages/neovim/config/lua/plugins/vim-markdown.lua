return {
  {
    'preservim/vim-markdown',
    ft = { 'markdown' },
    init = function()
      vim.g.vim_markdown_folding_style_pythonic = 1 -- Fold in a style like python-mode
      vim.g.vim_markdown_conceal = 0 -- Disable concealing
      vim.g.vim_markdown_conceal_code_blocks = 0
      vim.g.vim_markdown_strikethrough = 1 -- Enable strikethrough with ~~something~~
    end,
  },
}

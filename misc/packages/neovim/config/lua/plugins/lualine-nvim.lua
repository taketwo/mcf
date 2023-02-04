return {
  {
    'nvim-lualine/lualine.nvim',
    event = 'VeryLazy',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = {
      theme = 'onedark',
      sections = {
        lualine_y = {
          { 'progress', separator = '' },
          { 'searchcount' },
        },
      },
    },
    init = function()
      -- Do not show search count message when searching, it is a part of the statusline
      vim.o.shortmess = vim.o.shortmess .. 'S'
    end,
  },
}

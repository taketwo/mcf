return {
  {
    'nvim-lualine/lualine.nvim',
    event = 'VeryLazy',
    dependencies = {
      { 'AndreM222/copilot-lualine' },
      { 'echasnovski/mini.icons' },
    },
    opts = {
      theme = 'onedark',
      sections = {
        lualine_x = {
          { 'copilot', separator = ' ' },
          'encoding',
          'fileformat',
          'filetype',
        },
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

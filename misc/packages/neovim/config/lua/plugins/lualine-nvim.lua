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
          { 'overseer', separator = '' },
          { 'codecompanion', separator = '' },
          { 'copilot' },
          { 'encoding' },
          { 'fileformat' },
          { 'filetype' },
        },
        lualine_y = {
          { 'progress', separator = '' },
          { 'searchcount' },
        },
      },
      extensions = {
        'lazy',
        'mason',
        'nvim-dap-ui',
        'trouble',
        'overseer',
        'codecompanion',
      },
    },
    init = function()
      -- Do not show search count message when searching, it is a part of the statusline
      vim.o.shortmess = vim.o.shortmess .. 'S'
    end,
  },
}

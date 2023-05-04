return {
  {
    'lukas-reineke/indent-blankline.nvim',
    event = { 'BufReadPost', 'BufNewFile' },
    opts = {
      char_highlight_list = {},
      filetype_exclude = { 'help', 'alpha', 'dashboard', 'neo-tree', 'Trouble', 'lazy', 'mason' },
      show_current_context = false,
      show_trailing_blankline_indent = false,
      space_char_blankline = ' ',
    },
  },
}

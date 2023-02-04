return {
  {
    'lukas-reineke/indent-blankline.nvim',
    event = 'BufReadPost',
    opts = {
      char_highlight_list = {},
      filetype_exclude = { 'help', 'alpha', 'dashboard', 'neo-tree', 'Trouble', 'lazy' },
      show_current_context = true,
      show_trailing_blankline_indent = false,
      space_char_blankline = ' ',
    },
  },
}

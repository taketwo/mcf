return {
  {
    'dpayne/CodeGPT.nvim',
    dependencies = {
      { 'nvim-lua/plenary.nvim' },
      { 'MunifTanjim/nui.nvim' },
    },
    cmd = 'Chat',
    config = function()
      require('codegpt.config')
      vim.g.codegpt_popup_type = 'vertical'
      vim.g.codegpt_vertical_popup_size = '30%'
    end,
  },
}

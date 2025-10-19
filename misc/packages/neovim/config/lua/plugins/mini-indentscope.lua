return {
  {
    'nvim-mini/mini.indentscope',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      symbol = '│',
      options = {
        try_as_border = true, -- Get scope of function body by placing cursor on its header
      },
      draw = {
        animation = function() return 0 end,
      },
    },
    init = function()
      vim.api.nvim_create_autocmd('FileType', {
        pattern = {
          'Trouble',
          'alpha',
          'dashboard',
          'help',
          'lazy',
          'mason',
          'neo-tree',
          'notify',
          'sidekick_terminal',
          'snacks_notif',
          'snacks_terminal',
          'snacks_win',
        },
        callback = function() vim.b.miniindentscope_disable = true end,
      })
    end,
  },
}

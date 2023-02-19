return {
  {
    'echasnovski/mini.indentscope',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      symbol = '│',
      options = {
        try_as_border = true, -- Get scope of function body by placing cursor on its header
      },
    },
    config = function(_, opts)
      vim.api.nvim_create_autocmd('FileType', {
        pattern = { 'help', 'alpha', 'dashboard', 'neo-tree', 'Trouble', 'lazy', 'mason' },
        callback = function() vim.b.miniindentscope_disable = true end,
      })
      require('mini.indentscope').setup(opts)
    end,
  },
}

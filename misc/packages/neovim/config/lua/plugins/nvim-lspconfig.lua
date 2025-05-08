return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      { 'folke/neoconf.nvim', cmd = 'Neoconf', config = false, dependencies = { 'nvim-lspconfig' } },
      'mason.nvim',
      { 'mason-org/mason-lspconfig.nvim', config = false },
      'hrsh7th/cmp-nvim-lsp',
    },
    config = function()
      require('neoconf').setup()
      require('mcf.config.lsp').setup()
    end,
  },
}

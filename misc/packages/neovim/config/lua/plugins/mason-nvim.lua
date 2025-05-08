return {
  {
    'mason-org/mason.nvim',
    dependencies = {
      -- Config will be called from mcf.config.lsp.setup() after Mason and lspconfig are loaded
      { 'mason-org/mason-lspconfig.nvim', config = false },
    },
    cmd = { 'Mason', 'MasonUpdate' },
    build = ':MasonUpdate',
    opts = {},
  },
}

return {
  {
    'jose-elias-alvarez/null-ls.nvim',
    dependencies = { 'nvim-lua/plenary.nvim' },
    event = { 'BufReadPre', 'BufNewFile' },
    opts = function()
      local nls = require('null-ls')
      return {
        sources = {
          nls.builtins.formatting.black,
          nls.builtins.formatting.shfmt.with({
            extra_args = { '-i', '2', '-ci' },
          }),
          nls.builtins.formatting.stylua.with({
            extra_args = { '--config-path', vim.fn.stdpath('config') .. '/extras/stylua.toml' },
          }),
          nls.builtins.formatting.usort,
        },
        on_attach = require('config.lsp').on_attach,
      }
    end,
  },
}

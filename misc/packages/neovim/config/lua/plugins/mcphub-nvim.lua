return {
  {
    'ravitemer/mcphub.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
    },
    cmd = 'MCPHub',
    build = 'fnm exec --using node-nvim npm install --location=global mcp-hub@latest',
    opts = {
      config = vim.fn.stdpath('config') .. '/extras/mcp.json',
    },
    config = function(_, opts) require('mcphub').setup(opts) end,
  },
}

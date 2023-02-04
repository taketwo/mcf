return {
  {
    'folke/trouble.nvim',
    cmd = { 'TroubleToggle', 'Trouble' },
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = {
      action_keys = {
        previous = 'c',
        next = 't',
        open_tab = {}, -- never use tabs
      },
      auto_close = true,
      use_diagnostic_signs = true,
    },
    -- NOTE: Keymaps are defined in lspconfig.lua when LSP is active
  },
}

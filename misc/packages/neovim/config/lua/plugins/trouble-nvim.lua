return {
  {
    'folke/trouble.nvim',
    cmd = { 'TroubleToggle', 'Trouble' },
    keys = {
      {
        '[q',
        function()
          if require('trouble').is_open() then
            require('trouble').previous({ skip_groups = true, jump = true })
          else
            vim.cmd.cprev()
          end
        end,
        desc = 'Previous Trouble/Quickfix item',
      },
      {
        ']q',
        function()
          if require('trouble').is_open() then
            require('trouble').next({ skip_groups = true, jump = true })
          else
            vim.cmd.cnext()
          end
        end,
        desc = 'Next Trouble/Quickfix item',
      },
    },
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

return {
  {
    'folke/trouble.nvim',
    cmd = { 'Trouble' },
    keys = {
      {
        '[q',
        function()
          if require('trouble').is_open() then
            require('trouble').prev({ skip_groups = true, jump = true })
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
      auto_close = true, -- Auto close when there are no items
      focus = true, -- Focus the window when opened
      keys = {
        t = 'next',
        c = 'prev',
      },
    },
    -- NOTE: Keymaps are defined in lspconfig.lua when LSP is active
  },
}

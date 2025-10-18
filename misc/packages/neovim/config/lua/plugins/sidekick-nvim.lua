return {
  {
    'folke/sidekick.nvim',
    dependencies = {
      {
        'nvim-lualine/lualine.nvim',
        optional = true,
        event = 'VeryLazy',
        opts = function(_, opts)
          local icons = {
            Error = { ' ', 'DiagnosticError' },
            Inactive = { ' ', 'MsgArea' },
            Warning = { ' ', 'DiagnosticWarn' },
            Normal = { ' ', 'Special' },
          }
          table.insert(opts.sections.lualine_x, 2, {
            function()
              local status = require('sidekick.status').get()
              return status and vim.tbl_get(icons, status.kind, 1)
            end,
            cond = function() return require('sidekick.status').get() ~= nil end,
            color = function()
              local status = require('sidekick.status').get()
              local hl = status and (status.busy and 'DiagnosticWarn' or vim.tbl_get(icons, status.kind, 2))
              return { fg = Snacks.util.color(hl) }
            end,
          })
        end,
      },
    },
    opts = {
      cli = {
        mux = {
          backend = 'tmux',
          enabled = true,
        },
      },
    },
    keys = {
      {
        '<Tab>',
        function()
          -- If there is a next edit, jump to it; otherwise apply it if any
          if not require('sidekick').nes_jump_or_apply() then
            return '<Tab>' -- fallback to normal tab
          end
        end,
        expr = true,
        desc = 'Go to or apply NES',
      },
      {
        '<leader>At',
        function() require('sidekick.cli').send({ msg = '{this}' }) end,
        mode = { 'x', 'n' },
        desc = 'Send This',
      },
      {
        '<leader>Af',
        function() require('sidekick.cli').send({ msg = '{file}' }) end,
        desc = 'Send File',
      },
      {
        '<leader>Av',
        function() require('sidekick.cli').send({ msg = '{selection}' }) end,
        mode = { 'x' },
        desc = 'Send Visual Selection',
      },
      {
        '<leader>Ap',
        function() require('sidekick.cli').prompt() end,
        mode = { 'n', 'x' },
        desc = 'Sidekick Select Prompt',
      },
      -- Example of a keybinding to open Claude directly
      {
        '<Leader>ac',
        function() require('sidekick.cli').toggle({ name = 'claude', focus = true }) end,
        desc = 'Toggle ClaudeCode',
      },
    },
  },
}

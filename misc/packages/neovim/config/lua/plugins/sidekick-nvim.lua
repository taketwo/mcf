return {
  {
    'folke/sidekick.nvim',
    dependencies = {
      {
        'nvim-lualine/lualine.nvim',
        event = 'VeryLazy',
        opts = function(_, opts)
          -- TODO: Unify with copilot lualine status and possibly move to lualine plugin config
          table.insert(opts.sections.lualine_x, 2, {
            function()
              local status = require('sidekick.status').cli()
              return ' ' .. (#status > 1 and #status or '')
            end,
            cond = function() return #require('sidekick.status').cli() > 0 end,
            color = function() return { fg = Snacks.util.color('Debug') } end,
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
    config = function(_, opts)
      require('sidekick').setup(opts)
      Snacks.toggle({
        name = 'Next edit suggestions',
        get = function() return require('sidekick.nes').enabled end,
        set = function(state) require('sidekick.nes').enable(state) end,
      }):map('<leader>uN')
    end,
    keys = {
      -- Next Edit Suggestion keybindings
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
      -- CLI keybindings
      {
        '<Leader>ac',
        function() require('sidekick.cli').toggle({ name = 'claude', focus = true }) end,
        desc = 'Toggle ClaudeCode',
      },
      { '<Leader>as', '', mode = { 'n', 'x' }, desc = 'Send to ClaudeCode' },
      {
        '<Leader>ast',
        function() require('sidekick.cli').send({ msg = '{this}' }) end,
        mode = { 'x', 'n' },
        desc = 'Send this',
      },
      {
        '<Leader>asf',
        function() require('sidekick.cli').send({ msg = '{file}' }) end,
        desc = 'Send file',
      },
      {
        '<Leader>ass',
        function() require('sidekick.cli').send({ msg = '{selection}' }) end,
        mode = { 'x' },
        desc = 'Send visual selection',
      },
      {
        '<Leader>ap',
        function() require('sidekick.cli').prompt() end,
        mode = { 'n', 'x' },
        desc = 'Select Sidekick prompt',
      },
    },
  },
}

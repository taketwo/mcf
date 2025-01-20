return {
  {
    'olimorris/codecompanion.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
    },
    cmd = {
      'CodeCompanion',
      'CodeCompanionActions',
      'CodeCompanionChat',
      'CodeCompanionCmd',
    },
    keys = {
      { '<Leader>A', '', desc = 'AI support with CodeCompanion', mode = { 'n', 'v' } },
      { '<Leader>A<Space>', '<cmd>CodeCompanionChat Toggle<cr>', desc = 'Toggle chat window', mode = { 'n', 'v' } },
      { '<Leader>Aa', '<cmd>CodeCompanionActions<cr>', desc = 'Actions', mode = { 'n', 'v' } },
    },
    opts = {
      adapters = {
        copilot = function()
          return require('codecompanion.adapters').extend('copilot', {
            schema = {
              model = {
                default = 'claude-3.5-sonnet',
              },
            },
          })
        end,
      },
      strategies = {
        chat = {
          adapter = 'copilot',
          keymaps = {
            close = {
              modes = {
                n = 'q',
              },
            },
          },
        },
        inline = {
          adapter = 'copilot',
        },
        agent = {
          adapter = 'copilot',
        },
      },
    },
  },
}

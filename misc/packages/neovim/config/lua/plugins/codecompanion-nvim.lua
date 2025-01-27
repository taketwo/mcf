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
      {
        '<Space>',
        function() require('codecompanion').prompt('commit_message') end,
        desc = 'Generate commit message',
        ft = 'gitcommit',
      },
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
      prompt_library = {
        ['Commit Message'] = {
          strategy = 'inline',
          description = 'Generate a commit message',
          opts = {
            short_name = 'commit_message',
            auto_submit = true,
            placement = 'before|false',
          },
          prompts = {
            {
              role = 'user',
              content = function()
                return string.format(
                  [[You are an expert at following the Conventional Commit specification. Given the git diff listed below, please generate a commit message for me:

```diff
%s
```

Output only the commit message without any explanations and follow-up suggestions.
]],
                  vim.fn.system('git diff --no-ext-diff --staged')
                )
              end,
              opts = {
                contains_code = true,
              },
            },
          },
        },
      },
    },
    init = function()
      -- Expand 'cc' into 'CodeCompanion' in the command line
      vim.cmd([[cab cc CodeCompanion]])
    end,
  },
}

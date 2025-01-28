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
      display = {
        chat = {
          show_header_separator = true,
        },
      },
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
                local prompt, err = require('mcf.util.commits').generate_prompt()

                if err then
                  LazyVim.error(
                    string.format('Failed to generate commit message prompt: %s', err),
                    { title = 'Code Companion' }
                  )
                  return ''
                end
                LazyVim.debug(
                  string.format('Generated commit message prompt: %s', prompt),
                  { title = 'Code Companion' }
                )

                return prompt
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

return {
  {
    'olimorris/codecompanion.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      {
        'j-hui/fidget.nvim',
        opts = { notification = { window = { winblend = 0 } } },
      },
    },
    cmd = {
      'CodeCompanion',
      'CodeCompanionActions',
      'CodeCompanionChat',
      'CodeCompanionCmd',
    },
    keys = {
      { '<Leader>a', '', desc = 'AI support', mode = { 'n', 'v' } },
      { '<Leader>a<Space>', '<cmd>CodeCompanionChat Toggle<cr>', desc = 'Toggle chat window', mode = { 'n', 'v' } },
      { '<Leader>aa', '<cmd>CodeCompanionActions<cr>', desc = 'Actions', mode = { 'n', 'v' } },
      {
        '<C-g>',
        function()
          -- HACK: This is a workaround for Neogit overriding wrapping settings configured through ftplugin.
          vim.opt_local.wrap = true
          require('codecompanion').prompt('commit_message')
        end,
        desc = 'Generate commit message',
        ft = 'gitcommit',
        mode = { 'n', 'i' },
      },
    },
    opts = function()
      local user = (vim.env.USER or 'me'):gsub('^%l', string.upper)
      return {
        adapters = {
          copilot = function()
            return require('codecompanion.adapters').extend('copilot', {
              schema = {
                model = {
                  default = 'claude-3.7-sonnet',
                },
              },
            })
          end,
        },
        display = {
          chat = {
            icons = {
              pinned_buffer = ' ',
              watched_buffer = ' ',
            },
            intro_message = '',
          },
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
              options = {
                modes = { n = '?' },
                callback = function() require('which-key').show({ global = false }) end,
                description = 'Show CodeCompanion keymaps',
                hide = true,
              },
              -- Customized send command that stops insert mode before submitting the message
              send = {
                callback = function(chat)
                  vim.cmd('stopinsert')
                  chat:submit()
                end,
                index = 1,
                description = 'Send',
              },
            },
            roles = {
              llm = function(adapter)
                return string.format(
                  '  %s%s',
                  adapter.formatted_name,
                  adapter.parameters.model and ' (' .. adapter.parameters.model .. ')' or ''
                )
              end,
              user = '  ' .. user,
            },
            slash_commands = {
              ['buffer'] = {
                opts = {
                  provider = 'snacks',
                },
                keymaps = {
                  modes = {
                    n = '<LocalLeader>b',
                  },
                },
              },
              ['file'] = {
                opts = {
                  provider = 'snacks',
                },
                keymaps = {
                  modes = {
                    n = '<LocalLeader>f',
                  },
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
                content = require('mcf.util.prompts').generate_commit,
                opts = {
                  contains_code = true,
                },
              },
            },
          },
        },
        extensions = {
          mcphub = {
            callback = 'mcphub.extensions.codecompanion',
            opts = {
              show_result_in_chat = true,
              make_vars = true,
              make_slash_commands = true,
            },
          },
        },
      }
    end,
    init = function()
      require('mcf.extensions.codecompanion.fidget'):init()
      -- Expand 'cc' into 'CodeCompanion' in the command line
      vim.cmd([[cab cc CodeCompanion]])
      -- Expand 'ccc' into 'CodeCompanionCmd' in the command line
      vim.cmd([[cab ccc CodeCompanionCmd]])
    end,
  },
}

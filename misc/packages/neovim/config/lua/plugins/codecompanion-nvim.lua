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
      {
        '<Leader>ac',
        '<cmd>CodeCompanionChat Toggle<cr>',
        desc = 'Toggle CodeCompanion chat window',
        mode = { 'n', 'v' },
      },
      { '<Leader>aa', '<cmd>CodeCompanionActions<cr>', desc = 'Open CodeCompanion actions', mode = { 'n', 'v' } },
    },
    opts = function()
      local user = (vim.env.USER or 'me'):gsub('^%l', string.upper)
      return {
        adapters = {
          http = {
            copilot = function()
              return require('codecompanion.adapters').extend('copilot', {
                schema = {
                  model = {
                    default = 'claude-sonnet-4.5',
                  },
                },
              })
            end,
          },
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
        interactions = {
          chat = {
            tools = {
              opts = { wait_timeout = 3600000 }, -- Timeout waiting for user input/confirmation (1 hour)
            },
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
          ['Edit buffer'] = {
            interaction = 'chat',
            description = 'Edit the current buffer',
            opts = {
              alias = 'edit_buffer',
              auto_submit = false,
            },
            prompts = {
              {
                role = 'user',
                content = '#{buffer} @{insert_edit_into_file}\n\n ',
                opts = {
                  contains_code = true,
                },
              },
            },
          },
        },
        extensions = {},
      }
    end,
    init = function()
      -- Expand 'cc' into 'CodeCompanion' in the command line
      vim.cmd([[cab cc CodeCompanion]])
      -- Expand 'ccc' into 'CodeCompanionCmd' in the command line
      vim.cmd([[cab ccc CodeCompanionCmd]])
    end,
  },
}

return {
  {
    'milanglacier/minuet-ai.nvim',
    event = { 'BufReadPost', 'FileType gitcommit' },
    dependencies = { 'nvim-lua/plenary.nvim' },
    opts = {
      provider = 'openai_fim_compatible',
      context_window = 256,
      throttle = 1000,
      debounce = 200,
      provider_options = {
        openai_fim_compatible = {
          api_key = 'TERM',
          name = 'Ollama',
          end_point = 'http://localhost:11434/v1/completions',
          model = 'qwen2.5-coder:7b',
          optional = {
            max_tokens = 56,
            top_p = 0.9,
          },
        },
        openai_compatible = {
          api_key = 'TERM',
          name = 'Ollama',
          end_point = 'http://localhost:11434/v1/chat/completions',
          model = 'gemma4:31b-cloud',
          stream = true,
          few_shots = {},
          chat_input = {
            template = '{{{user_message}}}',
            user_message = function(context_before_cursor, _, _)
              local context = require('mcf.util.prompts').generate_commit()
              local prefix = vim.trim(context_before_cursor)
              if prefix ~= '' then
                return context
                  .. '\n\nThe user has already started typing the commit message: "'
                  .. prefix
                  .. '"\nComplete or suggest alternatives starting from where they left off.'
              end
              return context
            end,
          },
          system = {
            template = '{{{prompt}}}',
            prompt = [[
You are an expert at writing Git commit messages. You will be given context about a git repository and its staged changes. Suggest commit message completions.

Return completions separated by the marker <endCompletion>.
DO NOT include additional comments or markdown code block fences. Return the result directly.
Each completion should be a single subject line only, unless a body is clearly warranted.
]],
          },
          optional = {
            max_tokens = 512,
          },
        },
      },
      virtualtext = {
        -- NOTE: Minuet is currently not used for inline autocompletions, thus triggers and keymaps are disabled. Uncomment
        -- when/if switching to Minuet from Copilot.
        auto_trigger_ft = {},
        -- auto_trigger_ft = { '*' },
        -- auto_trigger_ignore_ft = { 'gitcommit' },
        keymap = {
          -- accept = '<Tab>',
          -- accept_line = '<S-Tab>',
          -- next = '<M-]>',
          -- prev = '<M-[>',
          -- dismiss = '<C-]>',
        },
      },
      presets = {
        gitcommit = {
          provider = 'openai_compatible',
          context_window = 8000,
          n_completions = 1,
        },
      },
    },
    config = function(_, opts)
      require('minuet').setup(opts)

      local function silent_change_preset(name)
        local orig = vim.notify
        vim.notify = function() end
        require('minuet').change_preset(name)
        vim.notify = orig
      end

      local augroup = vim.api.nvim_create_augroup('MinuetGitcommit', { clear = true })
      vim.api.nvim_create_autocmd('FileType', {
        group = augroup,
        pattern = 'gitcommit',
        callback = function()
          silent_change_preset('gitcommit')

          -- <C-g>: trigger completion and auto-accept the first suggestion
          vim.keymap.set('i', '<C-g>', function()
            local vt = require('minuet.virtualtext')
            vt.action.next()
            local attempts = 0
            local function try_accept()
              if vt.action.is_visible() then
                vt.action.accept()
              elseif attempts < 100 then
                attempts = attempts + 1
                vim.defer_fn(try_accept, 100)
              end
            end
            vim.defer_fn(try_accept, 100)
          end, { buffer = true, silent = true })
        end,
      })

      vim.api.nvim_create_autocmd('BufDelete', {
        group = augroup,
        pattern = 'COMMIT_EDITMSG',
        callback = function() silent_change_preset('original') end,
      })
    end,
  },
}

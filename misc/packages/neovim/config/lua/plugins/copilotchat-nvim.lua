return {
  {
    'CopilotC-Nvim/CopilotChat.nvim',
    branch = 'canary',
    dependencies = {
      { 'zbirenbaum/copilot.lua' },
      { 'nvim-lua/plenary.nvim' },
    },
    cmd = 'CopilotChat',
    keys = {
      { '<Leader>a', '', desc = 'AI support', mode = { 'n', 'v' } },
      { '<Leader>a<Space>', '<cmd>CopilotChatOpen<cr>', desc = 'Open Copilot chat', mode = { 'n', 'v' } },
      { '<Leader>ae', '<cmd>CopilotChatExplain<cr>', desc = 'Explain how this code works', mode = { 'n', 'v' } },
      { '<Leader>ar', '<cmd>CopilotChatReview<cr>', desc = 'Review this code', mode = { 'n', 'v' } },
      { '<Leader>af', '<cmd>CopilotChatFix<cr>', desc = 'Fix this code', mode = { 'n', 'v' } },
      { '<Leader>ao', '<cmd>CopilotChatOptimize<cr>', desc = 'Optimize this code', mode = { 'n', 'v' } },
      { '<Leader>ad', '<cmd>CopilotChatDocs<cr>', desc = 'Write documentation for this code', mode = { 'n', 'v' } },
      { '<Leader>aD', '<cmd>CopilotChatFixDiagnostic<cr>', desc = 'Assist with diagnostic issue', mode = { 'n', 'v' } },
      { '<Leader>at', '<cmd>CopilotChatTests<cr>', desc = 'Generate unit tests', mode = { 'n', 'v' } },
      { '<Leader>aC', '<cmd>CopilotChatCommit<cr>', desc = 'Write commit message for entire diff' },
      { '<Leader>ac', '<cmd>CopilotChatCommitStaged<cr>', desc = 'Write commit message for staged diff' },
      {
        '<Leader>aq',
        function()
          local input = vim.fn.input('Quick question: ')
          if input ~= '' then
            require('CopilotChat').ask(input, { selection = require('CopilotChat.select').visual })
          end
        end,
        desc = 'Quick question about this code',
        mode = { 'v' },
      },
      { '<C-s>', '<CR>', desc = 'Submit prompt', ft = 'copilot-chat', remap = true },
    },
    opts = {
      debug = false,
      mappings = {
        complete = {
          insert = '',
        },
      },
    },
    config = function(_, opts)
      require('CopilotChat').setup(opts)
      require('CopilotChat.integrations.cmp').setup()
      vim.api.nvim_create_autocmd('BufEnter', {
        pattern = 'copilot-chat',
        callback = function()
          vim.opt_local.relativenumber = false
          vim.opt_local.number = false
        end,
      })
    end,
  },
}

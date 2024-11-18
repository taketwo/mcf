local prompts = {
  RewriteUsingAlgorithms = '/COPILOT_GENERATE Rewrite this code using STL or Boost algorithms. If you can think of multiple ways to accomplish that, provide them all along with a discussion.',
}

return {
  {
    'CopilotC-Nvim/CopilotChat.nvim',
    branch = 'canary',
    dependencies = {
      { 'zbirenbaum/copilot.lua' },
      { 'nvim-lua/plenary.nvim' },
    },
    build = 'make tiktoken',
    cmd = 'CopilotChat',
    keys = {
      { '<Leader>a', '', desc = 'AI support', mode = { 'n', 'v' } },
      { '<Leader>a<Space>', '<cmd>CopilotChatOpen<cr>', desc = 'Open Copilot chat', mode = { 'n', 'v' } },
      {
        '<Leader>aa',
        function() require('CopilotChat.integrations.telescope').pick(require('CopilotChat.actions').prompt_actions()) end,
        desc = 'Actions',
        mode = { 'n', 'x' },
      },
      { '<Leader>ae', '<cmd>CopilotChatExplain<cr>', desc = 'Explain how this code works', mode = { 'n', 'v' } },
      { '<Leader>ar', '<cmd>CopilotChatReview<cr>', desc = 'Review this code', mode = { 'n', 'v' } },
      {
        '<Leader>aR',
        function() vim.diagnostic.reset(vim.api.nvim_get_namespaces()['copilot_review']) end,
        desc = 'Reset review diagnostics',
      },
      { '<Leader>af', '<cmd>CopilotChatFix<cr>', desc = 'Fix this code', mode = { 'n', 'v' } },
      { '<Leader>ao', '<cmd>CopilotChatOptimize<cr>', desc = 'Optimize this code', mode = { 'n', 'v' } },
      { '<Leader>ad', '<cmd>CopilotChatDocs<cr>', desc = 'Write documentation for this code', mode = { 'n', 'v' } },
      { '<Leader>aD', '<cmd>CopilotChatFixDiagnostic<cr>', desc = 'Assist with diagnostic issue', mode = { 'n', 'v' } },
      { '<Leader>at', '<cmd>CopilotChatTests<cr>', desc = 'Generate unit tests', mode = { 'n', 'v' } },
      { '<Leader>ac', '<cmd>CopilotChatCommit<cr>', desc = 'Write commit message' },
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
    opts = function()
      local user = vim.env.USER or 'User'
      user = user:sub(1, 1):upper() .. user:sub(2)
      return {
        debug = false,
        model = 'claude-3.5-sonnet',
        question_header = '  ' .. user .. ' ',
        answer_header = '  Copilot ',
        prompts = prompts,
      }
    end,
    config = function(_, opts)
      require('CopilotChat').setup(opts)
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

return {
  {
    'CopilotC-Nvim/CopilotChat.nvim',
    branch = 'canary',
    dependencies = {
      { 'zbirenbaum/copilot.lua' },
      { 'nvim-lua/plenary.nvim' },
      {
        'folke/which-key.nvim',
        opts = {
          defaults = {
            ['<Leader>a'] = {
              name = 'AI support',
              mode = { 'n', 'v' },
            },
          },
        },
      },
    },
    keys = {
      { '<Leader>a<Space>', '<cmd>CopilotChatOpen<cr>', desc = 'Open Copilot chat' },
      { '<Leader>ae', '<cmd>CopilotChatExplain<cr>', desc = 'Explain how this code works', mode = { 'n', 'v' } },
      { '<Leader>af', '<cmd>CopilotChatFix<cr>', desc = 'Fix this code', mode = { 'n', 'v' } },
      { '<Leader>at', '<cmd>CopilotChatTests<cr>', desc = 'Generate unit tests', mode = { 'n', 'v' } },
      { '<Leader>ao', '<cmd>CopilotChatOptimize<cr>', desc = 'Optimize this code', mode = { 'n', 'v' } },
      { '<Leader>ad', '<cmd>CopilotChatDocs<cr>', desc = 'Write documentation for this code', mode = { 'n', 'v' } },
      { '<Leader>ac', '<cmd>CopilotChatCommitStaged<cr>', desc = 'Write commit message for staged diff' },
      { '<Leader>aC', '<cmd>CopilotChatCommit<cr>', desc = 'Write commit message for entire diff' },
    },
    opts = {
      debug = false,
    },
  },
}

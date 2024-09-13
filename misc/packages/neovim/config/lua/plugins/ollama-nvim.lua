return {
  'nomnivore/ollama.nvim',
  dependencies = {
    { 'nvim-lua/plenary.nvim' },
  },
  cmd = { 'Ollama', 'OllamaModel' },
  keys = {
    { '<Leader>aO', '', desc = 'Ollama', mode = { 'n', 'v' } },
    -- Sample keybind for prompt menu. Note that the <c-u> is important for selections to work properly.
    {
      '<Leader>aO<Space>',
      ":<c-u>lua require('ollama').prompt()<cr>",
      desc = 'Prompt',
      mode = { 'n', 'v' },
    },
    -- Sample keybind for direct prompting. Note that the <c-u> is important for selections to work properly.
    {
      '<Leader>aOG',
      ":<c-u>lua require('ollama').prompt('Generate_Code')<cr>",
      desc = 'Generate Code',
      mode = { 'n', 'v' },
    },
  },
  opts = {
    model = 'codellama:7b-instruct',
  },
}

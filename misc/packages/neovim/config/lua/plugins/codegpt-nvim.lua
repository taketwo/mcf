return {
  {
    'dpayne/CodeGPT.nvim',
    dependencies = {
      { 'nvim-lua/plenary.nvim' },
      { 'MunifTanjim/nui.nvim' },
    },
    cmd = 'Chat',
    config = function()
      local key_filename = vim.fn.stdpath('config') .. '/extras/openai.key'
      if vim.fn.filereadable(key_filename) == 1 then
        vim.g['codegpt_openai_api_key'] = vim.fn.readfile(key_filename)[1]
      else
        vim.notify('CodeGPT.nvim: OpenAI API key not found', vim.log.levels.ERROR)
      end
    end,
  },
}

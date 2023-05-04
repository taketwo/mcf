return {
  {
    'jackMort/ChatGPT.nvim',
    dependencies = {
      'MunifTanjim/nui.nvim',
      'nvim-lua/plenary.nvim',
      'nvim-telescope/telescope.nvim',
    },
    cmd = { 'ChatGPT', 'ChatGPTActAs', 'ChatGPTEditWithInstructions', 'ChatGPTRun' },
    opts = {
      actions_paths = {},
    },
  },
}

return {
  {
    'linux-cultist/venv-selector.nvim',
    ft = 'python',
    cmd = { 'VenvSelect', 'VenvSelectCached', 'VenvSelectCurrent' },
    opts = {
      settings = {
        options = {
          notify_user_on_venv_activation = true,
        },
      },
    },
  },
}

return {
  {
    'linux-cultist/venv-selector.nvim',
    cmd = { 'VenvSelect', 'VenvSelectCached', 'VenvSelectCurrent' },
    opts = {
      -- Abuse venvwrapper to get the venvs from pipx
      venvwrapper_path = vim.fn.expand('$HOME') .. '/.local/pipx/venvs',
      name = {
        'venv',
        '.venv',
      },
    },
  },
}

return {
  -- List of supported settings: https://docs.astral.sh/ty/reference/editor-settings/
  settings = {
    ty = {
      configurationFile = vim.fn.stdpath('config') .. '/extras/ty.toml',
      disableLanguageServices = true,
      diagnosticMode = 'off',
    },
  },
}

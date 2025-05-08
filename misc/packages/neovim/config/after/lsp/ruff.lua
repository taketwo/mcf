return {
  init_options = {
    -- List of supported settings: https://docs.astral.sh/ruff/editors/settings/
    settings = {
      configuration = vim.fn.stdpath('config') .. '/extras/ruff.toml',
      fixAll = false,
      organizeImports = false,
    },
  },
  capabilities = {
    hoverProvider = false,
  },
}

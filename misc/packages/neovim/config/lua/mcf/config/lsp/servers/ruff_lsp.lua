return {
  init_options = {
    settings = {
      args = { '--config', vim.fn.stdpath('config') .. '/extras/ruff.toml' },
      fixAll = false,
      organizeImports = false,
    },
  },
  capabilities = {
    hoverProvider = false,
  },
}

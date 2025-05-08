return {
  settings = {
    Lua = {
      workspace = {
        checkThirdParty = 'Disable',
      },
      codeLens = {
        enable = false,
      },
      completion = {
        callSnippet = 'Replace',
      },
      doc = {
        privateName = { '^_' },
      },
      hint = {
        enable = true,
        arrayIndex = 'Disable',
      },
    },
  },
  capabilities = {
    documentFormattingProvider = false,
  },
}

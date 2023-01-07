require('mason-tool-installer').setup({
  ensure_installed = {
    'bash-language-server',
    'clangd',
    'lua-language-server',
    'jedi-language-server',
    'shellcheck',
    'stylua',
  },
})

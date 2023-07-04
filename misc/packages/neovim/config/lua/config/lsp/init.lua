local M = {}

function M.on_attach(client, buffer)
  require('config.lsp.keymaps').on_attach(client, buffer)
  require('lsp-inlayhints').on_attach(client, buffer)
end

return M

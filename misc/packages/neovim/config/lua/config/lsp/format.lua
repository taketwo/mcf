local M = {}

M.autoformat = true

function M._is_capability_disabled(client, capability)
  return
    client.config
    and client.config.capabilities
    and client.config.capabilities[capability] == false
end

function M.format()
  local buf = vim.api.nvim_get_current_buf()
  if vim.b.autoformat == false then return end
  local ft = vim.bo[buf].filetype
  local have_nls = #require('null-ls.sources').get_available(ft, 'NULL_LS_FORMATTING') > 0
  vim.lsp.buf.format({
    bufnr = buf,
    filter = function(client)
      -- If we have null-ls formatting client for this filetype, only use null-ls.
      if have_nls then return client.name == 'null-ls' end
      -- Otherwise, use all clients except null-ls that did not explicitly disable formatting provider.
      if M._is_capability_disabled(client, 'documentFormattingProvider') then return true end
      return client.name ~= 'null-ls'
    end,
  })
end

function M.on_attach(client, buffer)
  if M._is_capability_disabled(client, 'documentFormattingProvider') then return end
  if client.supports_method('textDocument/formatting') then
    -- Create an autocommand to format the buffer on save. If on_attach is called multiple times for
    -- the same buffer but different clients, this autocommand will be overwritten. This is okay
    -- because the callback is the same anyway.
    vim.api.nvim_create_autocmd('BufWritePre', {
      group = vim.api.nvim_create_augroup('LspFormat.' .. buffer, {}),
      buffer = buffer,
      callback = function()
        if M.autoformat then M.format() end
      end,
    })
  end
end

return M

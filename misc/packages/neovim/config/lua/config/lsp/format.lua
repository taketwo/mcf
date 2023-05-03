local Util = require('utils')

local M = {}

-- Global state, can be toggled using M.toggle({ global = true })
-- This is overridden by buffer-local state vim.b.autoformat
M.autoformat = true

function M._is_capability_disabled(client, capability)
  return client.config and client.config.capabilities and client.config.capabilities[capability] == false
end

---@param opts? {global?:boolean}
function M.toggle(opts)
  if opts and opts.global then
    M.autoformat = not M.autoformat
    Util.info((M.autoformat and 'Enabled' or 'Disabled') .. ' format on save globally', { title = 'Format' })
  else
    if vim.b.autoformat == nil then
      vim.b.autoformat = not M.autoformat
    else
      vim.b.autoformat = not vim.b.autoformat
    end
    Util.info((vim.b.autoformat and 'Enabled' or 'Disabled') .. ' format on save for this buffer', { title = 'Format' })
  end
end

---@param opts? {force?:boolean}
function M.format(opts)
  local buf = vim.api.nvim_get_current_buf()
  if not opts or not opts.force then
    if vim.b.autoformat == false or (vim.b.autoformat == nil and M.autoformat == false) then return end
  end
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
      callback = M.format,
    })
  end
end

return M

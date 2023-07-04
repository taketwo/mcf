local Util = require('utils')

local M = {}

-- Global state, can be toggled using M.toggle({ global = true })
-- This is overridden by buffer-local state vim.b.autoformat
M.autoformat = true

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

  local formatters = M.get_formatters(buf)
  local client_ids = vim.tbl_map(function(client) return client.id end, formatters.active)
  if #client_ids == 0 then return end
  vim.lsp.buf.format({
    bufnr = buf,
    filter = function(client) return vim.tbl_contains(client_ids, client.id) end,
  })
end

-- Get all LSP clients that support formatting.
-- When a null-ls formatter is available for the current filetype, only null-ls formatters are returned.
function M.get_formatters(bufnr)
  local ft = vim.bo[bufnr].filetype
  -- Check if we have any null-ls formatters for the current filetype
  local null_ls = package.loaded['null-ls'] and require('null-ls.sources').get_available(ft, 'NULL_LS_FORMATTING') or {}

  local ret = {
    ---@type lsp.Client[]
    active = {},
    ---@type lsp.Client[]
    available = {},
    null_ls = null_ls,
  }

  ---@type lsp.Client[]
  local clients = vim.lsp.get_active_clients({ bufnr = bufnr })
  for _, client in ipairs(clients) do
    if M.supports_formatting(client) then
      if (#null_ls > 0 and client.name == 'null-ls') or #null_ls == 0 then
        table.insert(ret.active, client)
      else
        table.insert(ret.available, client)
      end
    end
  end

  return ret
end

-- Check if a given client supports formatting and does not have it disabled in config.
---@param client lsp.Client
function M.supports_formatting(client)
  if
    client.config
    and client.config.capabilities
    and client.config.capabilities['documentFormattingProvider'] == false
  then
    return false
  end
  return client.supports_method('textDocument/formatting') or client.supports_method('textDocument/rangeFormatting')
end

function M.setup()
  -- Unconditionally create an autocommand to format buffers on save.
  -- Whether or not formatting is actually performed is decided in the format() function based on LSP client support and settings.
  vim.api.nvim_create_autocmd('BufWritePre', {
    group = vim.api.nvim_create_augroup('LspFormat', {}),
    callback = M.format,
  })
  -- Create a command to print a list of active and disabled formatters.
  vim.api.nvim_create_user_command('LspFormatInfo', M.info, {
    desc = 'Displays active and disabled language server formatters',
  })
end

function M.info()
  vim.print('Global autoformat setting: ' .. (M.autoformat and 'enabled' or 'disabled'))
  vim.print(
    'Buffer autoformat setting: '
      .. (vim.b.autoformat == nil and 'default' or (vim.b.autoformat and 'enabled' or 'disabled'))
  )

  local buf = vim.api.nvim_get_current_buf()
  local formatters = M.get_formatters(buf)

  vim.print('# Active:')
  for _, client in ipairs(formatters.active) do
    local line = '- ' .. client.name
    if client.name == 'null-ls' then
      line = line .. ' (' .. table.concat(vim.tbl_map(function(f) return f.name end, formatters.null_ls), ', ') .. ')'
    end
    vim.print(line)
  end

  if #formatters.available > 0 then
    vim.print('# Disabled:')
    for _, client in ipairs(formatters.available) do
      vim.print('- ' .. client.name)
    end
  end
end

return M

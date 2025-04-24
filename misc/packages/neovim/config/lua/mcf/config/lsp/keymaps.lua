local M = {}

-- Goto to the next error diagnostic, if present. Otherwise, goto to the next diagnostic of any severity.
local diagnostic_goto_next_priority = function()
  local opts = { severity = vim.diagnostic.severity.ERROR }
  require('lspsaga.diagnostic'):goto_next(vim.diagnostic.get_next(opts) and opts or nil)
end

-- Goto to the previous error diagnostic, if present. Otherwise, goto to the previous diagnostic of any severity.
local diagnostic_goto_prev_priority = function()
  local opts = { severity = vim.diagnostic.severity.ERROR }
  require('lspsaga.diagnostic'):goto_prev(vim.diagnostic.get_prev(opts) and opts or nil)
end

-- Goto next LSP item
-- LSP item is one of the following:
--   1. Trouble item (if Trouble is visible)
--   2. Error diagnostic (if present)
--   3. Non-error diagnostic
local goto_next = function()
  local Trouble = require('trouble')
  if Trouble.is_open() then
    Trouble.next({ jump = true })
  else
    diagnostic_goto_next_priority()
  end
end

-- Goto previous LSP item
-- LSP item is one of the following:
--   1. Trouble item (if Trouble is visible)
--   2. Error diagnostic (if present)
--   3. Non-error diagnostic
local goto_prev = function()
  local Trouble = require('trouble')
  if Trouble.is_open() then
    Trouble.prev({ jump = true })
  else
    diagnostic_goto_prev_priority()
  end
end

---@type LazyKeysLspSpec[]|nil
M._keys = {
  { '<', goto_prev, desc = 'Go to previous LSP item' },
  { '>', goto_next, desc = 'Go to next LSP item' },
  { '[d', function() require('lspsaga.diagnostic'):goto_prev() end, desc = 'Previous diagnostic' },
  { ']d', function() require('lspsaga.diagnostic'):goto_next() end, desc = 'Next diagnostic' },
  {
    '<C-k>',
    function() return vim.lsp.buf.signature_help() end,
    mode = { 'i', 'n' },
    desc = 'Display LSP signature help',
    has = 'signatureHelp',
  },
  { '<Leader>lD', '<cmd>Lspsaga show_line_diagnostics<cr>', desc = 'Line diagnostics' },
  {
    '<Leader>lN',
    function()
      vim.ui.input({ prompt = 'New Name:' }, function(input)
        if not input or #input == 0 then return end
        vim.lsp.buf.rename(input)
      end)
    end,
    desc = 'Rename symbol (no placeholder)',
    mode = { 'n', 'v' },
    has = 'rename',
  },
  {
    '<Leader>lR',
    function() Snacks.rename.rename_file() end,
    desc = 'Rename file',
    mode = { 'n' },
    has = { 'workspace/didRenameFiles', 'workspace/willRenameFiles' },
  },
  { '<Leader>lT', '<cmd>Trouble diagnostics toggle<cr>', desc = 'Show workspace diagnostics in Trouble' },
  { '<Leader>la', vim.lsp.buf.code_action, desc = 'Code action', mode = { 'n', 'v' }, has = 'codeAction' },
  { '<Leader>ld', '<cmd>Lspsaga peek_definition<cr>', desc = 'Definition preview' },
  { '<Leader>lf', '<cmd>Lspsaga lsp_finder<cr>', desc = 'Finder' },
  { '<Leader>li', Snacks.picker.lsp_config, desc = 'Show info' },
  { '<Leader>ln', vim.lsp.buf.rename, desc = 'Rename symbol', mode = { 'n', 'v' }, has = 'rename' },
  { '<Leader>lr', '<cmd>Telescope lsp_references<cr>', desc = 'References', has = 'references' },
  { '<Leader>ls', vim.lsp.buf.signature_help, desc = 'Signature help', has = 'signatureHelp' },
  { '<Leader>lt', '<cmd>Trouble diagnostics toggle filter.buf=0<cr>', desc = 'Show document diagnostics in Trouble' },
  { '<leader>lL', vim.lsp.codelens.refresh, desc = 'Refresh and display Codelens', mode = { 'n' }, has = 'codeLens' },
  { '<leader>ll', vim.lsp.codelens.run, desc = 'Run Codelens', mode = { 'n', 'v' }, has = 'codeLens' },
  {
    '<Leader>.S',
    '<cmd>Telescope lsp_dynamic_workspace_symbols<cr>',
    desc = 'Jump to symbol (workspace)',
    has = 'workspaceSymbol',
  },
  { '<Leader>.s', '<cmd>Telescope lsp_document_symbols<cr>', desc = 'Jump to symbol', has = 'documentSymbol' },
  { 'K', function() vim.lsp.buf.hover() end, desc = 'Display LSP hover information', has = 'hover' },
  { 'gD', '<cmd>Trouble lsp_declarations<cr>', desc = 'Go to declaration', has = 'declaration' },
  { 'gI', '<cmd>Trouble lsp_implementations<cr>', desc = 'Go to implementation', has = 'implementation' },
  { 'gd', '<cmd>Trouble lsp_definitions<cr>', desc = 'Go to definition', has = 'definition' },
  { 'gr', '<cmd>Trouble lsp_references<cr>', desc = 'Go to references', has = 'references' },
  { 'gT', '<cmd>Trouble lsp_type_definitions<cr>', desc = 'Go to type definition', has = 'typeDefinition' },
  { '<Leader>ui', function() Snacks.toggle.inlay_hints():toggle() end, desc = 'Toggle inlay hints', has = 'inlayHint' },
}

-- Do not set LSP keymaps for these filetypes
M._filetype_blacklist = { 'ctrlsf' }

-- Check if any of the LSP clients for the buffer support the given method
-- Copied from LazyVim
---@param method string|string[]
function M.has(buffer, method)
  if type(method) == 'table' then
    for _, m in ipairs(method) do
      if M.has(buffer, m) then return true end
    end
    return false
  end
  method = method:find('/') and method or 'textDocument/' .. method
  local clients = LazyVim.lsp.get_clients({ bufnr = buffer })
  for _, client in ipairs(clients) do
    if client:supports_method(method) then return true end
  end
  return false
end

---@return LazyKeysLsp[]
function M.resolve(buffer)
  local Keys = require('lazy.core.handler.keys')
  if not Keys.resolve then return {} end
  local server_configs = require('mcf.config.lsp').get_server_configs()
  local spec = M._keys
  local clients = LazyVim.lsp.get_clients({ bufnr = buffer })
  for _, client in ipairs(clients) do
    local maps = server_configs[client.name] and server_configs[client.name].keys or {}
    vim.list_extend(spec, maps)
  end
  return Keys.resolve(spec)
end

function M.on_attach(_, buffer)
  require('which-key').add({
    buffer = buffer,
    {
      { '<Leader>l', group = 'LSP', mode = { 'n', 'v' } },
    },
  })
  if vim.tbl_contains(M._filetype_blacklist, vim.bo.filetype) then return end

  -- The rest is copied from LazyVim
  local Keys = require('lazy.core.handler.keys')
  local keymaps = M.resolve(buffer)
  for _, keys in pairs(keymaps) do
    local has = not keys.has or M.has(buffer, keys.has)
    local cond = not (keys.cond == false or ((type(keys.cond) == 'function') and not keys.cond()))

    if has and cond then
      local opts = Keys.opts(keys)
      opts.cond = nil
      opts.has = nil
      opts.silent = opts.silent ~= false
      opts.buffer = buffer
      vim.keymap.set(keys.mode or 'n', keys.lhs, keys.rhs, opts)
    end
  end
end

return M

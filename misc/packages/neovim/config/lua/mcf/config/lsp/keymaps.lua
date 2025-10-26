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

---LSP keymaps organized by language server name.
---The special key '*' contains generic keymaps applied to all LSP servers. Server-specific keys
---(e.g., 'clangd', 'ruff') contain keymaps that are only applied in buffers where that specific
---server is active.
---@type table<string, LazyKeysLspSpec[]>|nil
M._keys = {
  ['*'] = {
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
    {
      '<Leader>ui',
      function() Snacks.toggle.inlay_hints():toggle() end,
      desc = 'Toggle inlay hints',
      has = 'inlayHint',
    },
  },
  ['clangd'] = {
    { '<LocalLeader>\\', '<cmd>LspClangdSwitchSourceHeader<cr>', desc = 'Alternate between source/header' },
  },
}

---Copied from LazyVim
---@param filter vim.lsp.get_clients.Filter
---@param spec LazyKeysLspSpec[]
function M.set(filter, spec)
  local Keys = require('lazy.core.handler.keys')
  for _, keys in pairs(Keys.resolve(spec)) do
    ---@cast keys LazyKeysLsp
    if keys.cond == nil or keys.cond() then
      local filters = {} ---@type vim.lsp.get_clients.Filter[]
      if keys.has then
        local methods = type(keys.has) == 'string' and { keys.has } or keys.has --[[@as string[] ]]
        for _, method in ipairs(methods) do
          method = method:find('/') and method or ('textDocument/' .. method)
          filters[#filters + 1] = vim.tbl_extend('force', vim.deepcopy(filter), { method = method })
        end
      else
        filters[#filters + 1] = filter
      end

      for _, f in ipairs(filters) do
        local opts = Keys.opts(keys)
        ---@cast opts snacks.keymap.set.Opts
        opts.lsp = f
        Snacks.keymap.set(keys.mode or 'n', keys.lhs, keys.rhs, opts)
      end
    end
  end
end

function M.setup()
  for server, keys in pairs(M._keys) do
    M.set(server == '*' and {} or { name = server }, keys)
  end
end

return M

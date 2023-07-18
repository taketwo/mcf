local M = {}

local format = function() require('config.lsp.format').format({ force = true }) end

-- Jump to the next error diagnostic. If there are no error diagnostics, jump to the next diagnostic of any severity.
local diagnostic_jump_next = function()
  local opts = { severity = vim.diagnostic.severity.ERROR }
  require('lspsaga.diagnostic'):goto_next(vim.diagnostic.get_next(opts) and opts or nil)
end

-- Jump to the previous error diagnostic. If there are no error diagnostics, jump to the previous diagnostic of any severity.
local diagnostic_jump_prev = function()
  local opts = { severity = vim.diagnostic.severity.ERROR }
  require('lspsaga.diagnostic'):goto_prev(vim.diagnostic.get_prev(opts) and opts or nil)
end

M._keys = {
  { '<', diagnostic_jump_prev, desc = 'Go to previous diagnostic' },
  { '>', diagnostic_jump_next, desc = 'Go to next diagnostic' },
  {
    '<F5>',
    vim.lsp.buf.signature_help,
    mode = 'i',
    desc = 'Display LSP signature help',
    has = 'signatureHelp',
  },
  { '<Leader>lD', '<cmd>Lspsaga show_line_diagnostics<cr>', desc = 'Line diagnostics' },
  { '<Leader>lf', '<cmd>Lspsaga lsp_finder<cr>', desc = 'Finder' },
  { '<Leader>ld', '<cmd>Lspsaga peek_definition<cr>', desc = 'Definition preview' },
  { '<Leader>li', '<cmd>LspInfo<cr>', desc = 'Show info' },
  { '<Leader>ls', vim.lsp.buf.signature_help, desc = 'Signature help', has = 'signatureHelp' },
  {
    '<Leader>lt',
    '<cmd>TroubleToggle workspace_diagnostics<cr>',
    desc = 'Show workspace diagnostics in Trouble',
  },
  { '<Leader>la', vim.lsp.buf.code_action, desc = 'Code action', mode = { 'n', 'v' }, has = 'codeAction' },
  { '<Leader>ln', vim.lsp.buf.rename, desc = 'Rename symbol', mode = { 'n', 'v' }, has = 'rename' },
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
  { 'K', vim.lsp.buf.hover, desc = 'Display LSP hover information', has = 'hover' },
  { 'gD', vim.lsp.buf.declaration, desc = 'Go to declaration', has = 'declaration' },
  {
    'gI',
    function() require('telescope.builtin').lsp_implementations({ reuse_win = true }) end,
    desc = 'Go to implementation',
    has = 'implementation',
  },
  {
    'gd',
    function() require('telescope.builtin').lsp_definitions({ reuse_win = true }) end,
    desc = 'Goto to definition',
    has = 'definition',
  }, -- TODO: Or use Trouble?
  { 'gr', '<cmd>Trouble lsp_references<cr>', desc = 'Go to references', has = 'references' },
  { 'gt', '<cmd>Trouble lsp_type_definitions<cr>', desc = 'Go to type definition', has = 'typeDefinition' },
  { '<Leader>ui', function() vim.lsp.inlay_hint(0) end, desc = 'Toggle inlay hints', has = 'inlayHint' },
  { '<F2>', format, desc = 'Format document', has = 'documentFormatting' },
  { '<F2>', format, desc = 'Format range', mode = 'v', has = 'documentRangeFormatting' },
}

function M.on_attach(client, buffer)
  for _, keys in ipairs(M._keys) do
    if not keys.has or client.server_capabilities[keys.has .. 'Provider'] then
      local opts = require('lazy.core.handler.keys').opts(keys)
      opts.has = nil
      opts.silent = true
      opts.buffer = buffer
      vim.keymap.set(keys.mode or 'n', keys[1], keys[2], opts)
    end
  end
end

return M

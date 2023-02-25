local M = {}

M._keys = {
  { '<', '<cmd>Lspsaga diagnostic_jump_prev<cr>', desc = 'Go to previous diagnostic' },
  { '>', '<cmd>Lspsaga diagnostic_jump_next<cr>', desc = 'Go to next diagnostic' },
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
  { 'K', vim.lsp.buf.hover, desc = 'Display LSP hover information', has = 'hover' },
  { 'gD', vim.lsp.buf.declaration, desc = 'Go to declaration', has = 'declaration' },
  { 'gI', '<cmd>Trouble lsp_implementations<cr>', desc = 'Go to implementation', has = 'implementation' },
  { 'gd', '<cmd>Telescope lsp_definitions<cr>', desc = 'Goto to definition', has = 'definition' }, -- TODO: Or use Trouble?
  { 'gr', '<cmd>Trouble lsp_references<cr>', desc = 'Go to references', has = 'references' },
  { 'gt', '<cmd>Trouble lsp_type_definitions<cr>', desc = 'Go to type definition', has = 'typeDefinition' },
  {
    '<Leader>ui',
    function() require('lsp-inlayhints').toggle() end,
    desc = 'Toggle inlay hints',
    has = 'inlayHint',
  },
  { '<F2>', require('config.lsp.format').format, desc = 'Format document', has = 'documentFormatting' },
  { '<F2>', require('config.lsp.format').format, desc = 'Format range', mode = 'v', has = 'documentRangeFormatting' },
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

local Util = require('mcf.util')

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

M._keys = {
  { '<', diagnostic_goto_prev_priority, desc = 'Go to previous diagnostic' },
  { '>', diagnostic_goto_next_priority, desc = 'Go to next diagnostic' },
  { '[d', function() require('lspsaga.diagnostic'):goto_prev() end, desc = 'Previous diagnostic' },
  { ']d', function() require('lspsaga.diagnostic'):goto_next() end, desc = 'Next diagnostic' },
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
  { '<leader>ll', vim.lsp.codelens.run, desc = 'Run Codelens', mode = { 'n', 'v' }, has = 'codeLens' },
  { '<leader>lL', vim.lsp.codelens.refresh, desc = 'Refresh and display Codelens', mode = { 'n' }, has = 'codeLens' },
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
  { '<Leader>ui', function() Util.toggle.inlay_hints() end, desc = 'Toggle inlay hints', has = 'inlayHint' },
}

-- Do not set LSP keymaps for these filetypes
M._filetype_blacklist = { 'ctrlsf' }

function M.on_attach(client, buffer)
  require('which-key').register({
    ['<Leader>l'] = {
      name = 'LSP',
      mode = { 'n', 'v' },
    },
  }, { buffer = buffer })
  if vim.tbl_contains(M._filetype_blacklist, vim.bo.filetype) then return end
  for _, keys in ipairs(M._keys) do
    if
      not keys.has
      or client.server_capabilities[keys.has .. 'Provider']
      or client.supports_method('textDocument/' .. keys.has)
    then
      local opts = require('lazy.core.handler.keys').opts(keys)
      opts.has = nil
      opts.silent = true
      opts.buffer = buffer
      vim.keymap.set(keys.mode or 'n', keys[1], keys[2], opts)
    end
  end
end

return M

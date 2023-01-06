-- Neodev has to be set up before lspconfig.
-- Normally, this should be done using Packer sequencing feature.
-- However, in this case, asking Packer to load Neodev before lspconfig leads to issues with file discovery.
require('neodev').setup()

local lspconfig = require('lspconfig')

local on_attach = function(client, bufnr)
  require('which-key').register({
    K = { '<cmd>lua vim.lsp.buf.hover()<cr>', 'Display LSP hover information' },
    ['<c-k>'] = { '<cmd>lua.vim.lsp.buf.signature_help()<cr>', 'Display LSP signature help' },
    g = {
      d = { '<cmd>lua vim.lsp.buf.definition()<cr>', 'Go to definition' },
      D = { '<cmd>lua vim.lsp.buf.declaration()<cr>', 'Go to declaration' },
      t = { '<cmd>lua vim.lsp.buf.type_definition()<cr>', 'Go to type definition' },
    },
    ['<Leader>'] = {
      l = {
        name = 'LSP',
        a = { '<cmd>lua vim.lsp.buf.code_action()<cr>', 'Code action' },
        n = { '<cmd>lua vim.lsp.buf.rename()<cr>', 'Rename symbol' },
      },
    },
  }, { buffer = bufnr })

  -- if client.server_capabilities.document_formatting then
  -- buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  -- elseif client.server_capabilities.document_range_formatting then
  -- buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  -- end

  -- Autocommands

  -- Send diagnostics to the location list (without opening it)
  vim.api.nvim_exec(
    [[
    augroup lsp_publish_diagnostics
      autocmd! * <buffer>
      autocmd DiagnosticChanged * lua vim.diagnostic.setloclist({open=false})
    augroup END
  ]],
    false
  )

  -- Highlight entity under cursor
  if client.server_capabilities.documentHighlightProvider then
    vim.api.nvim_exec(
      [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
      false
    )
  end
end

local cmp_capabilities = require('cmp_nvim_lsp').default_capabilities()
local clangd_capabilities = cmp_capabilities
clangd_capabilities.offsetEncoding = 'utf-16'

  cmd = { 'clangd', '--background-index', '--completion-style=detailed', '--cross-file-rename' },
lspconfig.clangd.setup({
  filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'gtest.cpp' },
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = clangd_capabilities,
})

lspconfig.bashls.setup({
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = cmp_capabilities,
})

lspconfig.jedi_language_server.setup({
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  capabilities = cmp_capabilities,
})

lspconfig.sumneko_lua.setup({
  on_attach = on_attach,
  flags = {
    debounce_text_changes = 150,
  },
  settings = {
    Lua = {
      completion = {
        callSnippet = 'Replace',
      },
    },
  },
})

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  -- Enable underline, use default values
  underline = true,
  -- Enable virtual text, override spacing to 4
  virtual_text = {
    spacing = 4,
  },
  -- Use a function to dynamically turn signs off
  -- and on, using buffer local variables
  signs = function(bufnr, client_id)
    local ok, result = pcall(vim.api.nvim_buf_get_var, bufnr, 'show_signs')
    -- No buffer local variable set, so just enable by default
    if not ok then return true end

    return result
  end,
  -- Disable a feature
  update_in_insert = false,
})

vim.fn.sign_define('DiagnosticSignError', { text = '', texthl = 'DiagnosticSignError' })
vim.fn.sign_define('DiagnosticSignWarn', { text = '', texthl = 'DiagnosticSignWarn' })
vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
vim.fn.sign_define('DiagnosticSignHint', { text = '', texthl = 'DiagnosticSignHint' })

function _G.open_lsp_log()
  local path = vim.lsp.get_log_path()
  vim.cmd('edit ' .. path)
end

vim.cmd('command! -nargs=0 LspLog call v:lua.open_lsp_log()')

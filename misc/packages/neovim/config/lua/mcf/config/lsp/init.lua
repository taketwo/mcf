local M = {}

---Display the capabilities of all LSP clients attached to the current buffer
---Shows results in a floating window with markdown formatting
function M.capabilities()
  local current_buf = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ bufnr = current_buf })

  local lines = {}

  for _, client in pairs(clients) do
    local capabilities = {}
    for key, value in pairs(client.server_capabilities) do
      if value and key:find('Provider') then
        local capability = key:gsub('Provider$', '')
        table.insert(capabilities, '* ' .. capability)
      end
    end
    table.sort(capabilities) -- sorts alphabetically
    table.insert(lines, '# ' .. client.name)
    if #capabilities == 0 then
      table.insert(lines, '* (none)')
    else
      vim.list_extend(lines, capabilities)
    end
  end

  LazyVim.info(table.concat(lines, '\n'), { title = 'LSP Capabilities' })
end

---Set up the LSP configuration for the Neovim instance
---Configures formatters, keymaps, diagnostics appearance, and loads server configurations
function M.setup()
  -- Register LSP formatter
  LazyVim.format.register(LazyVim.lsp.formatter())
  -- Register keymaps for all known LSP servers
  require('mcf.config.lsp.keymaps').setup()

  -- Setup appearance of diagnostics
  vim.diagnostic.config({
    underline = true,
    update_in_insert = false,
    virtual_text = { spacing = 4 },
    severity_sort = true,
    signs = {
      text = {
        [vim.diagnostic.severity.ERROR] = '',
        [vim.diagnostic.severity.WARN] = '',
        [vim.diagnostic.severity.INFO] = '',
        [vim.diagnostic.severity.HINT] = '',
      },
    },
  })

  -- Add global capabilities
  vim.lsp.config('*', {
    capabilities = {
      workspace = {
        fileOperations = {
          didRename = true,
          willRename = true,
        },
      },
    },
  })

  require('mason-lspconfig').setup()

  vim.api.nvim_create_user_command(
    'LspCapabilities',
    function() M.capabilities() end,
    { desc = 'Show capabilities of LSP clients attached to the current buffer' }
  )
  vim.api.nvim_create_user_command(
    'LspLogClear',
    function() os.remove(vim.lsp.get_log_path()) end,
    { desc = 'Clear LSP log file' }
  )
end

return M

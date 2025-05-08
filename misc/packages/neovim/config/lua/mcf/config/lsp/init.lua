local M = {}

---Attach LSP-related keymaps to a buffer when an LSP client connects
---@param client table The LSP client that attached
---@param buffer integer The buffer number that the client attached to
function M.on_attach(client, buffer) require('mcf.config.lsp.keymaps').on_attach(client, buffer) end

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

  -- Setup dynamic capability registration provided by LazyVim
  LazyVim.lsp.setup()

  -- Setup keymaps on attach and when new capability is registered
  LazyVim.lsp.on_attach(function(client, buffer) M.on_attach(client, buffer) end)
  LazyVim.lsp.on_dynamic_capability(M.on_attach)

  -- TODO: Enable inlay hints and code lens for selected LSP servers

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

  local has_cmp, cmp_nvim_lsp = pcall(require, 'cmp_nvim_lsp')
  vim.lsp.config('*', {
    capabilities = vim.tbl_deep_extend('force', {}, has_cmp and cmp_nvim_lsp.default_capabilities() or {}, {
      -- Global capabilities
      workspace = {
        fileOperations = {
          didRename = true,
          willRename = true,
        },
      },
    }),
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

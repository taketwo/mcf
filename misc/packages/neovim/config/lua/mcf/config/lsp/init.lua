local M = {}

function M.on_attach(client, buffer) require('mcf.config.lsp.keymaps').on_attach(client, buffer) end

function M.capabilities()
  local current_buf = vim.api.nvim_get_current_buf()
  local clients = vim.lsp.get_clients({ bufnr = current_buf })

  local lines = {}

  for _, client in pairs(clients) do
    if client.name ~= 'null-ls' then
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
  end

  LazyVim.info(table.concat(lines, '\n'), { title = 'LSP Capabilities' })
end

function M.setup()
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

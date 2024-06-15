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
  local capabilities = vim.tbl_deep_extend(
    'force',
    {},
    vim.lsp.protocol.make_client_capabilities(),
    has_cmp and cmp_nvim_lsp.default_capabilities() or {},
    {
      -- Global capabilities
      workspace = {
        fileOperations = {
          didRename = true,
          willRename = true,
        },
      },
    }
  )

  -- LSP server settings
  -- TODO: Move to separate files
  local servers = {
    bashls = {},
    clangd = {
      cmd = {
        'clangd',
        '--background-index',
        '--clang-tidy',
        '--completion-style=bundled', -- TODO: Check if detailed style is better
        '--function-arg-placeholders', -- TODO: Understand whether this is useful
      },
      filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'gtest.cpp' },
      capabilities = {
        offsetEncoding = 'utf-16',
      },
      -- Some of the supported options are listed at: https://clangd.llvm.org/extensions
      init_options = {
        usePlaceholders = true, -- TODO: Understand whether this is useful
        completeUnimported = true, -- TODO: Understand whether this is useful
      },
    },
    jedi_language_server = {},
    lua_ls = {
      settings = {
        Lua = {
          workspace = {
            checkThirdParty = 'Disable',
          },
          codeLens = {
            enable = false,
          },
          completion = {
            callSnippet = 'Replace',
          },
          doc = {
            privateName = { '^_' },
          },
          hint = {
            enable = true,
            arrayIndex = 'Disable',
          },
        },
      },
      capabilities = {
        documentFormattingProvider = false,
      },
    },
    ruff_lsp = {
      init_options = {
        settings = {
          args = { '--config', vim.fn.stdpath('config') .. '/extras/ruff.toml' },
          fixAll = false,
          organizeImports = false,
        },
      },
      capabilities = {
        hoverProvider = false,
      },
    },
    taplo = {},
    typos_lsp = {
      init_options = {
        -- Assign 'warning' severity to discovered typos
        diagnosticSeverity = 'Warning',
      },
    },
    yamlls = {
      settings = {
        redhat = {
          telemetry = {
            enabled = false,
          },
        },
        yaml = {
          keyOrdering = false,
          format = {
            enable = true,
          },
          validate = {
            enable = true,
          },
        },
      },
    },
  }

  local function setup(server)
    local server_opts = vim.tbl_deep_extend('force', {
      capabilities = vim.deepcopy(capabilities),
    }, servers[server] or {})
    -- NOTE: LazyVim optionally called custom server setup function here.
    -- As we do not have any use cases for this, the step was removed.
    require('lspconfig')[server].setup(server_opts)
  end

  local available = vim.tbl_keys(require('mason-lspconfig.mappings.server').lspconfig_to_package)

  local ensure_installed = {} ---@type string[]
  for server, server_opts in pairs(servers) do
    if server_opts then
      server_opts = server_opts == true and {} or server_opts
      if server_opts.enabled ~= false then
        -- Run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
        if server_opts.mason == false or not vim.tbl_contains(available, server) then
          setup(server)
        else
          ensure_installed[#ensure_installed + 1] = server
        end
      end
    end
  end

  require('mason-lspconfig').setup({ ensure_installed = ensure_installed, handlers = { setup } })

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

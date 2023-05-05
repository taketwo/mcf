return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      { 'folke/neoconf.nvim', cmd = 'Neoconf', config = true },
      { 'folke/neodev.nvim', opts = { experimental = { pathStrict = true } } },
      'mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'hrsh7th/cmp-nvim-lsp',
    },
    config = function()
      vim.diagnostic.config({
        underline = true,
        update_in_insert = false,
        virtual_text = { spacing = 4 },
        severity_sort = true,
      })
      vim.fn.sign_define('DiagnosticSignError', { text = '', texthl = 'DiagnosticSignError' })
      vim.fn.sign_define('DiagnosticSignWarn', { text = '', texthl = 'DiagnosticSignWarn' })
      vim.fn.sign_define('DiagnosticSignInfo', { text = '', texthl = 'DiagnosticSignInfo' })
      vim.fn.sign_define('DiagnosticSignHint', { text = '', texthl = 'DiagnosticSignHint' })

      -- LSP server settings
      local servers = {
        bashls = {},
        clangd = {
          cmd = { 'clangd', '--background-index', '--completion-style=bundled', '--clang-tidy' },
          filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'gtest.cpp' },
          capabilities = {
            offsetEncoding = 'utf-16',
          },
        },
        jedi_language_server = {},
        lua_ls = {
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              completion = {
                callSnippet = 'Replace',
              },
              hint = {
                enable = true,
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
      }

      local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

      local function setup(server)
        local server_opts = servers[server] or {}
        server_opts.capabilities = vim.tbl_deep_extend('force', capabilities, server_opts.capabilities or {})
        server_opts.on_attach = require('config.lsp').on_attach
        require('lspconfig')[server].setup(server_opts)
      end

      local available = vim.tbl_keys(require('mason-lspconfig.mappings.server').lspconfig_to_package)

      local ensure_installed = {}
      for server, server_opts in pairs(servers) do
        if server_opts then
          server_opts = server_opts == true and {} or server_opts
          -- Run manual setup if mason=false or if this is a server that cannot be installed with mason-lspconfig
          if server_opts.mason == false or not vim.tbl_contains(available, server) then
            setup(server)
          else
            ensure_installed[#ensure_installed + 1] = server
          end
        end
      end

      require('mason-lspconfig').setup({ ensure_installed = ensure_installed })
      require('mason-lspconfig').setup_handlers({ setup })
    end,
  },
}

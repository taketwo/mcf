return {
  {
    'neovim/nvim-lspconfig',
    event = { 'BufReadPre', 'BufNewFile' },
    dependencies = {
      { 'folke/neodev.nvim', opts = { experimental = { pathStrict = true } } },
      'mason.nvim',
      'williamboman/mason-lspconfig.nvim',
      'hrsh7th/cmp-nvim-lsp',
    },
    config = function()
      -- TODO: Implement auto-formatting support
      -- TODO: Setup keymaps based on server capabilities (see LazyVim for an example)
      local on_attach = function(_, bufnr)
        require('which-key').register({
          K = { '<cmd>lua vim.lsp.buf.hover()<cr>', 'Display LSP hover information' },
          ['<c-k>'] = { '<cmd>lua.vim.lsp.buf.signature_help()<cr>', 'Display LSP signature help' },
          g = {
            d = { '<cmd>lua vim.lsp.buf.definition()<cr>', 'Go to definition' },
            D = { '<cmd>lua vim.lsp.buf.declaration()<cr>', 'Go to declaration' },
            r = { '<cmd>Trouble lsp_references<cr>', 'Go to references' },
            t = { '<cmd>Trouble lsp_type_definitions<cr>', 'Go to type definition' },
          },
          ['<Leader>'] = {
            l = {
              name = 'LSP',
              a = { '<cmd>lua vim.lsp.buf.code_action()<cr>', 'Code action' },
              n = { '<cmd>lua vim.lsp.buf.rename()<cr>', 'Rename symbol' },
              t = { '<cmd>TroubleToggle workspace_diagnostics<cr>', 'Show workspace diagnostics in Trouble' },
            },
          },
          ['>'] = { vim.diagnostic.goto_next, 'Go to next diagnostic' },
          ['<'] = { vim.diagnostic.goto_prev, 'Go to previous diagnostic' },
        }, { buffer = bufnr })

        -- Autocommands
        -- Send diagnostics to the location list (without opening it)
        -- TODO: Use vim.api.nvim_create_autocmd
        vim.api.nvim_exec(
          [[
    augroup lsp_publish_diagnostics
      autocmd! * <buffer>
      autocmd DiagnosticChanged * lua vim.diagnostic.setloclist({open=false})
    augroup END
]],
          false
        )
      end

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
        clangd = {
          cmd = { 'clangd', '--background-index', '--completion-style=bundled', '--clang-tidy' },
          filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'gtest.cpp' },
          capabilities = {
            offsetEncoding = 'utf-16',
          },
        },
        bashls = {},
        jedi_language_server = {},
        sumneko_lua = {
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              completion = {
                callSnippet = 'Replace',
              },
            },
          },
        },
      }

      local capabilities = require('cmp_nvim_lsp').default_capabilities(vim.lsp.protocol.make_client_capabilities())

      local function setup(server)
        local server_opts = servers[server] or {}
        server_opts.capabilities = vim.tbl_deep_extend('force', capabilities, server_opts.capabilities or {})
        server_opts.on_attach = on_attach
        require('lspconfig')[server].setup(server_opts)
      end

      local available = require('mason-lspconfig').get_available_servers()

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

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
      local on_attach = function(client, buffer)
        local Keys = {
          { '<', '<cmd>Lspsaga diagnostic_jump_prev<cr>', desc = 'Go to previous diagnostic' },
          { '>', '<cmd>Lspsaga diagnostic_jump_next<cr>', desc = 'Go to next diagnostic' },
          {
            '<C-k>',
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
          { '<leader>la', vim.lsp.buf.code_action, desc = 'Code action', mode = { 'n', 'v' }, has = 'codeAction' },
          { '<leader>ln', vim.lsp.buf.rename, desc = 'Rename symbol', mode = { 'n', 'v' }, has = 'rename' },
          { 'K', vim.lsp.buf.hover, desc = 'Display LSP hover information', has = 'hover' },
          { 'gD', vim.lsp.buf.declaration, desc = 'Go to declaration' },
          { 'gI', '<cmd>Trouble lsp_implementations<cr>', desc = 'Go to implementation' },
          { 'gd', '<cmd>Telescope lsp_definitions<cr>', desc = 'Goto to definition' }, -- TODO: Or use Trouble?
          { 'gr', '<cmd>Trouble lsp_references<cr>', desc = 'Go to references' },
          { 'gt', '<cmd>Trouble lsp_type_definitions<cr>', desc = 'Go to type definition' },
        }
        for _, keys in ipairs(Keys) do
          if not keys.has or client.server_capabilities[keys.has .. 'Provider'] then
            local opts = require('lazy.core.handler.keys').opts(keys)
            opts.has = nil
            opts.silent = true
            opts.buffer = buffer
            vim.keymap.set(keys.mode or 'n', keys[1], keys[2], opts)
          end
        end
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
        lua_ls = {
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

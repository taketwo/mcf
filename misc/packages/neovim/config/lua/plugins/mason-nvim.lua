return {
  {
    'williamboman/mason.nvim',
    cmd = 'Mason',
    build = ':MasonUpdate',
    opts = {
      -- List of tools to install with Mason
      -- All configured LSP servers will be added to this list automatically and do not need to be included here explicitly
      ensure_installed = {
        'bash-debug-adapter',
        'black',
        'cpptools',
        'gersemi',
        'json-lsp',
        'shellcheck',
        'shfmt',
        'stylua',
        'usort',
      },
    },
    config = function(_, opts)
      require('mason').setup(opts)
      local registry = require('mason-registry')

      -- Setup handler for successful installation of packages.
      -- It triggers FileType event to possibly load this newly installed LSP server (copied from LazyVim)
      registry:on('package:install:success', function()
        vim.defer_fn(
          function()
            require('lazy.core.handler.event').trigger({
              event = 'FileType',
              buf = vim.api.nvim_get_current_buf(),
            })
          end,
          100
        )
      end)

      -- Automatically refresh registry and install tools from the list above (copied from LazyVim)
      -- This is triggered every time LSP config is loaded
      registry.refresh(function()
        for _, tool in ipairs(opts.ensure_installed) do
          local p = registry.get_package(tool)
          if not p:is_installed() then p:install() end
        end
      end)
    end,
  },
}

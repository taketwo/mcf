return {
  {
    'williamboman/mason.nvim',
    cmd = { 'Mason', 'MasonUpdate' },
    build = ':MasonUpdate',
    config = function(_, opts)
      require('mason').setup(opts)

      -- Setup handler for successful installation of packages.
      -- It triggers FileType event to possibly load this newly installed LSP server (copied from LazyVim)
      require('mason-registry'):on('package:install:success', function()
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
    end,
  },
}

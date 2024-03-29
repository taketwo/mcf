return {
  {
    'williamboman/mason.nvim',
    cmd = 'Mason',
    build = ':MasonUpdate',
    opts = {
      ensure_installed = {
        'bash-debug-adapter',
        'bash-language-server',
        'black',
        'clangd',
        'cpptools',
        'gersemi',
        'jedi-language-server',
        'json-lsp',
        'lua-language-server',
        'ruff-lsp',
        'shellcheck',
        'shfmt',
        'stylua',
        'taplo',
        'typos-lsp',
        'usort',
        'yaml-language-server',
      },
    },
    config = function(_, opts)
      require('mason').setup(opts)
      -- Automatically refresh registry and install tools from the list above (copied from LazyVim)
      -- This is triggered every time LSP config is loaded
      local registry = require('mason-registry')
      registry.refresh(function()
        for _, tool in ipairs(opts.ensure_installed) do
          local p = registry.get_package(tool)
          if not p:is_installed() then p:install() end
        end
      end)
    end,
  },
}

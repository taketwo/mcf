return {
  {
    'williamboman/mason.nvim',
    cmd = 'Mason',
    opts = {
      ensure_installed = {
        'bash-language-server',
        'clangd',
        'lua-language-server',
        'jedi-language-server',
        'shellcheck',
        'stylua',
      },
    },
    config = function(_, opts)
      require('mason').setup(opts)
      -- Automatically install tools from the list above, copied from LazyVim
      -- TODO: This probably is not called during bootsrapping, figure out how to do so
      local mr = require('mason-registry')
      for _, tool in ipairs(opts.ensure_installed) do
        local p = mr.get_package(tool)
        if not p:is_installed() then p:install() end
      end
    end,
  },
}

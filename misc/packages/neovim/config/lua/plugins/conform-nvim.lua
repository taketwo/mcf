return {
  {
    'stevearc/conform.nvim',
    cmd = 'ConformInfo',
    init = function()
      -- This initialization function was copied from LazyVim and should be kept in sync with it.
      -- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/plugins/formatting.lua
      require('lazyvim.util').on_very_lazy(function()
        require('lazyvim.util').format.register({
          name = 'conform.nvim',
          priority = 100,
          primary = true,
          format = function(buf)
            local plugin = require('lazy.core.config').plugins['conform.nvim']
            local Plugin = require('lazy.core.plugin')
            local opts = Plugin.values(plugin, 'opts', false)
            require('conform').format(require('lazyvim.util').merge({}, opts.format, { bufnr = buf }))
          end,
          sources = function(buf)
            local ret = require('conform').list_formatters(buf)
            return vim.tbl_map(function(v) return v.name end, ret)
          end,
        })
      end)
    end,
    opts = {
      format = {
        timeout_ms = 3000,
      },
      log_level = vim.log.levels.DEBUG,
      formatters_by_ft = {
        lua = { 'stylua' },
        python = { 'black', 'usort' },
        sh = { 'shfmt' },
        xml = { 'xmllint' },
        cmake = { 'gersemi' },
      },
      formatters = {
        injected = { options = { ignore_errors = true } },
        shfmt = {
          prepend_args = { '-i', '2', '-ci' },
        },
        stylua = {
          prepend_args = { '--config-path', vim.fn.stdpath('config') .. '/extras/stylua.toml' },
        },
      },
    },
  },
}

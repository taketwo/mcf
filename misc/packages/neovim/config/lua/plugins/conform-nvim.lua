return {
  {
    'stevearc/conform.nvim',
    cmd = 'ConformInfo',
    init = function()
      -- This initialization function was copied from LazyVim and should be kept in sync with it.
      -- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/plugins/formatting.lua
      -- The only modification is to avoid using local conform_opts variable and set set timeout_ms
      -- option directly in the call to require('conform').format().
      require('lazyvim.util').on_very_lazy(function()
        require('lazyvim.util').format.register({
          name = 'conform.nvim',
          priority = 100,
          primary = true,
          format = function(buf)
            require('conform').format(require('lazyvim.util').merge({
              timeout_ms = 3000,
            }, { bufnr = buf }))
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
        sh = { 'shfmt' },
        -- NOTE: We would like to have usort as a secondary formatter for Python, but conform.nvim
        -- does not support it yet.
        python = { 'black' },
        -- NOTE: We would like to use gersemi as the primary formatter for CMake, but conform.nvim
        -- does not support it yet.
        -- cmake = { 'gersemi' },
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

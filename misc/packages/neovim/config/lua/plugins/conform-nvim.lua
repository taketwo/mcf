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
          format = function(buf) require('conform').format({ bufnr = buf }) end,
          sources = function(buf)
            local ret = require('conform').list_formatters(buf)
            ---@param v conform.FormatterInfo
            return vim.tbl_map(function(v) return v.name end, ret)
          end,
        })
      end)
    end,
    opts = {
      default_format_opts = {
        timeout_ms = 3000,
        async = false, -- not recommended to change
        quiet = false, -- not recommended to change
        lsp_format = 'fallback', -- not recommended to change
      },
      log_level = vim.log.levels.DEBUG,
      formatters_by_ft = {
        cmake = { 'gersemi' },
        lua = { 'stylua' },
        proto = { 'clang-format' },
        python = { 'black', 'usort' },
        sh = { 'shfmt' },
        xml = { 'xmllint' },
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

return {
  {
    'zbirenbaum/copilot.lua',
    event = 'VeryLazy',
    opts = {
      suggestion = {
        enabled = true,
        auto_trigger = true,
        debounce = 75,
        keymap = {
          accept = '<Tab>',
          accept_word = '<S-Tab>',
          next = '<M-]>',
          prev = '<M-[>',
          dismiss = '<C-]>',
        },
      },
      copilot_node_command = vim.g.node_path .. '/node',
    },
    config = function(_, opts)
      require('copilot').setup(opts)
      -- HACK: work-around for https://github.com/neovim/neovim/issues/31262
      local Util = require('copilot.util')
      local language_for_file_type = Util.language_for_file_type
      Util.language_for_file_type = function(ft) return language_for_file_type(ft or '') end
    end,
  },
}

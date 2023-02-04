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
      copilot_node_command = vim.fn.expand('$FNM_DIR') .. '/aliases/node17/bin/node',
    },
  },
}

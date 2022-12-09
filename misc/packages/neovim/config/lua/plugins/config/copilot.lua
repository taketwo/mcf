vim.defer_fn(function()
  require('copilot').setup({
    suggestion = {
      enabled = true,
      auto_trigger = true,
      debounce = 75,
      keymap = {
       accept = "<Tab>",
       next = "<M-]>",
       prev = "<M-[>",
       dismiss = "<C-]>",
      },
    },
    copilot_node_command = vim.fn.expand("$FNM_DIR") .. "/aliases/node17/bin/node"
  })
end, 100)

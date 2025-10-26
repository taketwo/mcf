vim.keymap.set({ 'n' }, '<LocalLeader>r', function() Snacks.debug.run() end, { desc = 'Run code in current buffer' })
vim.keymap.set({ 'x' }, '<LocalLeader>r', function() Snacks.debug.run() end, { desc = 'Run selected code' })

require('which-key').register({
  ['<F8>'] = { '<cmd>write<bar>VimuxRunLastCommand<cr>', 'Run last command in Vimux', mode = { 'n', 'i' } },
  ['<F8><F8>'] = { '<cmd>write<bar>VimuxRunLastCommand<bar>VimuxZoomRunner<cr>', 'Run last command in Vimux and zoom' },
})

-- Use marked pane as a runner
-- Caveat: if a different pane is marked after Vimux was used for the first time, the runner will not be updated
vim.g.VimuxRunnerQuery = { pane = '{marked}' }
vim.g.VimuxLastCommand = 'fc -s'

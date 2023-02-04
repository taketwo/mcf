return {
  {
    'preservim/vimux',
    cmd = { 'VimuxRunCommand', 'VimuxRunLastCommand' },
    keys = {
      { '<F8>', '<cmd>write<bar>VimuxRunLastCommand<cr>', mode = { 'n', 'i' }, desc = 'Run last command in Vimux' },
      {
        '<F8><F8>',
        '<cmd>write<bar>VimuxRunLastCommand<bar>VimuxZoomRunner<cr>',
        desc = 'Run last command in Vimux and zoom',
      },
    },
    init = function()
      -- Use marked pane as a runner
      -- Caveat: if a different pane is marked after Vimux was used for the first time, the runner will not be updated
      vim.g.VimuxRunnerQuery = { pane = '{marked}' }
      vim.g.VimuxLastCommand = 'fc -s'
    end,
  },
}

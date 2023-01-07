require('which-key').register({
  ['<Leader>'] = {
    l = {
      name = 'LSP',
      t = { '<cmd>TroubleToggle workspace_diagnostics<cr>', 'Shod workspace diagnostics in Trouble' },
    },
  },
})

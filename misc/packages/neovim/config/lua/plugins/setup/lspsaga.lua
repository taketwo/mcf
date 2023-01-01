require('which-key').register({
  ['<Leader>'] = {
    l = {
      name = 'LSP',
      D = { '<cmd>Lspsaga show_line_diagnostics<cr>', 'Line diagnostics' },
      d = { '<cmd>Lspsaga peek_definition<cr>', 'Definition preview' },
      f = { '<cmd>Lspsaga lsp_finder<cr>', 'Finder' },
    },
  },
})

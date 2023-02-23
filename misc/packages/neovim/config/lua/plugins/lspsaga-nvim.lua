return {
  {
    'glepnir/lspsaga.nvim',
    event = "BufRead", -- TODO: Load only when LSP is attached
    cmd = { 'Lspsaga' },
    keys = {
      { '<Leader>lD', '<cmd>Lspsaga show_line_diagnostics<cr>', 'Line diagnostics' },
      { '<Leader>ld', '<cmd>Lspsaga peek_definition<cr>', 'Definition preview' },
      { '<Leader>lf', '<cmd>Lspsaga lsp_finder<cr>', 'Finder' },
    },
    opts = {
      symbol_in_winbar = {
        enable = false,
      },
    },
  },
}

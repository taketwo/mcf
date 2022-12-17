require('which-key').register{
  ['<Leader>'] = {
    ['.'] = {
      name = 'Telescope',
      ['.'] = { '<cmd>Telescope find_files<cr>', 'Open file' },
      b = { '<cmd>Telescope buffers<cr>', 'Jump to buffer' },
      g = { '<cmd>Telescope live_grep<cr>', 'Live grep' },
      h = { '<cmd>Telescope help_tags<cr>', 'Open help' },
      s = { '<cmd>Telescope lsp_document_symbols<cr>', 'Jump to symbol' },
    },
    g = {
      name = 'Git',
      L = { '<cmd>Telescope git_commits<cr>', 'Log (everything)' },
      l = { '<cmd>Telescope git_bcommits<cr>', 'Log (buffer only)' },
      s = { '<cmd>Telescope git_status<cr>', 'Status' },
    },
    l = {
      name = 'LSP',
      r = { '<cmd>Telescope lsp_references<cr>', 'References' },
    },
    z = {
      ['='] = { '<cmd>Telescope spell_suggest<cr>', 'Spelling suggestions' },
    },
  },
  ['<Space>'] = { '<cmd>Telescope buffers<cr>', 'Jump to buffer' },
}

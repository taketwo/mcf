return {
  {
    'sindrets/diffview.nvim',
    cmd = { 'DiffviewOpen', 'DiffviewFileHistory' },
    keys = {
      { '<Leader>d', '', desc = 'Diffs' },
      { '<Leader>dH', '<cmd>DiffviewFileHistory %<cr>', desc = 'Open diff view file history (current file)' },
      { '<Leader>dO', '<cmd>DiffviewOpen %<cr>', desc = 'Open diff view against current index (current file)' },
      { '<Leader>dc', '<cmd>DiffviewClose<cr>', desc = 'Close diff view' },
      { '<Leader>dh', '<cmd>DiffviewFileHistory<cr>', desc = 'Open diff view file history' },
      { '<Leader>do', '<cmd>DiffviewOpen<cr>', desc = 'Open diff view against current index' },
      { '<Leader>dp', '<cmd>DiffviewToggleFiles<cr>', desc = 'Toggle diff view files panel' },
    },
    opts = {
      enhanced_diff_hl = true,
    },
  },
}

return {
  {
    'jreybert/vimagit',
    cmd = { 'Magit' },
    keys = { { '<Leader>gc', '<cmd>Magit<cr>', desc = 'Commit' } },
    init = function()
      vim.g.magit_jump_next_hunk = '>'
      vim.g.magit_jump_prev_hunk = '<'
    end,
  },
}

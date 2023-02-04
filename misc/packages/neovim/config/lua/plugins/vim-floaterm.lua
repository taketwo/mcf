return {
  {
    'voldikss/vim-floaterm',
    cmd = { 'FloatermNew', 'FloatermToggle', 'FloatermNext', 'FloatermPrev' },
    keys = { '<F6>' },
    init = function()
      vim.g.floaterm_keymap_toggle = '<F6>'
      vim.api.nvim_exec([[ hi FloatermBorder guibg=orange guifg=cyan ]], false)
    end,
  },
}

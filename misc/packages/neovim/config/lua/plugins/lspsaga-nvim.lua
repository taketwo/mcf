return {
  {
    'glepnir/lspsaga.nvim',
    cmd = { 'Lspsaga' },
    opts = {
      symbol_in_winbar = {
        enable = false,
      },
      beacon = {
        enable = false, -- disable annoying flashing
      },
      lightbulb = {
        enable = false,
      },
      diagnostic = {
        -- Disable automatic display of code action preview window after a jump.
        -- It's still possible to see the preview by pressing <C-f> and <C-b>.
        auto_preview = false,
      },
    },
  },
}

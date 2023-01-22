require('which-key').setup({
  plugins = {
    presets = {
      -- Disable operators to avoid delay on pressing "c"
      -- Instead, we enable them selectively under the 'operators' key
      operators = false,
    },
  },
  operators = {
    k = 'Change', -- does not have effect
    y = 'Yank',
    d = 'Delete',
    v = 'Visual'
  },
})

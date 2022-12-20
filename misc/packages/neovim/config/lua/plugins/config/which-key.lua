require('which-key').setup({
  plugins = {
    presets = {
      -- Temporary disable operators to avoid delay on pressing "c"
      operators = false,
    },
  },
  triggers_blacklist = {
    i = { 'c', 't', 'h', 'n', '.' },
    v = { 'c', 't', 'h', 'n' },
  },
})

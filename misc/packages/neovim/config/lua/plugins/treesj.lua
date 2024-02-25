return {
  {
    'Wansmer/treesj',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    keys = {
      { 'gS', function() require('treesj').toggle() end, desc = 'Toggle split/join for node under cursor' },
    },
    cmd = { 'TSJToggle', 'TSJSplit', 'TSJJoin' },
    opts = {
      use_default_keymaps = false,
    },
  },
}

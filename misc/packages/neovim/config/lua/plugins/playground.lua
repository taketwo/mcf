return {
  {
    'nvim-treesitter/playground',
    event = 'BufReadPost',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    -- NOTE: Config is in nvim-treesitter.lua
  },
}

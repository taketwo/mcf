return {
  {
    'nvim-treesitter/nvim-treesitter-textobjects',
    event = 'BufReadPost',
    dependencies = { 'nvim-treesitter/nvim-treesitter' },
    -- NOTE: Config is in nvim-treesitter.lua
  },
}

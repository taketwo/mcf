return {
  {
    'nvim-tree/nvim-tree.lua',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    cmd = { 'NvimTreeToggle', 'NvimTreeOpen' },
    keys = {
      { '<F12>', '<cmd>NvimTreeToggle<cr>', desc = 'Toggle NvimTree' },
    },
    opts = {}, -- Plugin configuration fails if this is not present
  },
}

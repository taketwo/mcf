return {
  'NeogitOrg/neogit',
  cmd = { 'Neogit' },
  keys = { { '<Leader>gn', '<cmd>Neogit<cr>', desc = 'Neogit' } },
  dependencies = {
    'nvim-lua/plenary.nvim',
    -- "sindrets/diffview.nvim",        -- optional - Diff integration
  },
  ---@type NeogitConfig
  opts = {
    graph_style = 'unicode',
    kind = 'vsplit',
    mappings = {
      status = {
        ['>'] = 'GoToNextHunkHeader',
        ['<'] = 'GoToPreviousHunkHeader',
      },
    },
  },
  config = true,
}

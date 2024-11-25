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
      popup = {
        -- Reassign c/t to uppercase to avoid conflicts with navigation keymaps
        ['C'] = 'CommitPopup',
        ['c'] = false,
        ['T'] = 'TagPopup',
        ['t'] = false,
      },
      status = {
        ['>'] = 'GoToNextHunkHeader',
        ['<'] = 'GoToPreviousHunkHeader',
      },
    },
  },
  config = true,
}

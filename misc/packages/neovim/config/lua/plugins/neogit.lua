return {
  'NeogitOrg/neogit',
  cmd = { 'Neogit' },
  keys = { { '<Leader>gn', '<cmd>Neogit<cr>', desc = 'Neogit' } },
  dependencies = { 'nvim-lua/plenary.nvim' },
  ---@type NeogitConfig
  opts = {
    graph_style = 'unicode',
    kind = 'vsplit',
    commit_editor = {
      staged_diff_split_kind = 'vsplit',
    },
    mappings = {
      commit_editor = {
        ['<c-n>'] = 'NextMessage',
        ['<c-h>'] = 'PrevMessage',
        ['<c-r>'] = 'ResetMessage',
        ['<m-n>'] = false,
        ['<m-p>'] = false,
        ['<m-r>'] = false,
      },
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

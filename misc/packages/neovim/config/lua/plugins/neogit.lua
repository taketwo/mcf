return {
  'NeogitOrg/neogit',
  cmd = { 'Neogit' },
  keys = { { '<Leader>gn', '<cmd>Neogit<cr>', desc = 'Neogit' } },
  dependencies = { 'nvim-lua/plenary.nvim' },
  ---@type NeogitConfig
  opts = {
    graph_style = 'unicode',
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
        -- Reassign s/u to uppercase to be in line with vimagit mappings
        ['S'] = 'Stage',
        ['s'] = false,
        ['U'] = 'Unstage',
        ['u'] = false,
      },
    },
  },
  config = true,
}

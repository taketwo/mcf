return {
  'NeogitOrg/neogit',
  cmd = { 'Neogit' },
  keys = {
    { '<Leader>g<Space>', '<cmd>Neogit<cr>', desc = 'Neogit' },
    { '<Leader>gc', '<cmd>Neogit commit<cr>', desc = 'Commit' },
  },
  dependencies = { 'nvim-lua/plenary.nvim' },
  ---@type NeogitConfig
  opts = {
    remember_settings = false,
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
  init = function()
    vim.api.nvim_create_autocmd('FileType', {
      pattern = 'NeogitStatus',
      callback = function()
        vim.keymap.set('n', 'L', 'VS', { buffer = true, remap = true, desc = 'Stage current line' })
      end,
    })
  end,
}

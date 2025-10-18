return {
  {
    'christoomey/vim-tmux-navigator',
    keys = {
      { '<M-h>', '<cmd>TmuxNavigateLeft<cr>', desc = 'Go to left window or pane', mode = { 'n', 't' } },
      { '<M-n>', '<cmd>TmuxNavigateRight<cr>', desc = 'Go to right window or pane', mode = { 'n', 't' } },
      { '<M-c>', '<cmd>TmuxNavigateUp<cr>', desc = 'Go to upper window or pane', mode = { 'n', 't' } },
      { '<M-t>', '<cmd>TmuxNavigateDown<cr>', desc = 'Go to lower window or pane', mode = { 'n', 't' } },
    },
    init = function()
      vim.g.tmux_navigator_no_mappings = 1
      vim.g.tmux_navigator_disable_when_zoomed = 1
    end,
  },
}

return {
  {
    'taketwo/vim-ros',
    event = { 'BufReadPre', 'BufNewFile' },
    init = function() vim.g.ros_disable_warnings = 1 end,
  },
}

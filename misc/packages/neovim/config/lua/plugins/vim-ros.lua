return {
  {
    'taketwo/vim-ros',
    event = 'BufRead',
    init = function() vim.g.ros_disable_warnings = 1 end,
  },
}

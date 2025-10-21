return {
  {
    'stevearc/overseer.nvim',
    commit = '9334595f850a301ef8bd1d19fc46dca688f677b3',
    cmd = { 'OverseerRun', 'OverseerToggle' },
    keys = {
      { '<Leader>o', '', desc = 'Overseer' },
      { '<Leader>oc', '<cmd>OverseerClearCache<cr>', desc = 'Clear task cache' },
      { '<Leader>od', '<cmd>OverseerQuickAction dispose<cr>', desc = 'Dispose most recent task' },
      { '<Leader>oq', '<cmd>OverseerQuickAction<cr>', desc = 'Run action on most recent task' },
      { '<Leader>or', '<cmd>OverseerRun<cr>', desc = 'Run task' },
      { '<Leader>ot', '<cmd>OverseerToggle<cr>', desc = 'Toggle panel' },
      {
        '<F9>',
        function()
          vim.cmd('write')
          vim.cmd('OverseerRun BUILD')
        end,
        desc = 'Run build',
        silent = false,
        mode = { 'n', 'i' },
      },
    },
    opts = {
      templates = { 'builtin', 'catkin.build' },
      -- Disable patching of nvim-dap to support preLaunchTask and postDebugTask
      -- We do not use this feature and it causes error notifications on startup
      dap = false,
      log = {
        {
          type = 'notify',
          level = vim.log.levels.INFO,
        },
        {
          type = 'file',
          filename = 'overseer.log',
          level = vim.log.levels.DEBUG,
        },
      },
    },
  },
}

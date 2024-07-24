return {
  {
    'stevearc/overseer.nvim',
    cmd = { 'OverseerRun', 'OverseerToggle' },
    opts = {
      templates = { 'builtin', 'catkin/build' },
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

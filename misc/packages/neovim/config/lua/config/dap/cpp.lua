local M = {}

function M.setup()
  local dap = require('dap')
  dap.configurations.cpp = {
    {
      name = 'Launch file',
      type = 'cppdbg',
      request = 'launch',
      program = function()
        return vim.fn.input({
          prompt = 'Path to executable: ',
          default = vim.fn.getcwd() .. '/',
          completion = 'file',
        })
      end,
      cwd = '${workspaceFolder}',
      stopAtEntry = true,
      setupCommands = {
        {
          description = 'Enable pretty-printing for gdb',
          text = '-enable-pretty-printing',
          ignoreFailures = true,
        },
      },
    },
  }
  dap.adapters.cppdbg = {
    type = 'executable',
    command = 'OpenDebugAD7',
    id = 'cppdbg',
  }
end

return M

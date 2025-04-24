local M = {}

function M.setup()
  local dap = require('dap')
  dap.configurations.cpp = {
    setmetatable({
      name = 'Launch ROS executable',
      type = 'cppdbg',
      request = 'launch',
      cwd = '${workspaceFolder}',
      stopAtEntry = true,
      setupCommands = {
        {
          description = 'Enable pretty-printing for gdb',
          text = '-enable-pretty-printing',
          ignoreFailures = true,
        },
      },
    }, {
      __call = function(config)
        local ros_package_name = vim.b.ros_package_name
        local ros_package_workspace = vim.b.ros_package_workspace
        local construct_executable_path = function(executable_name)
          return ros_package_workspace .. '/devel/lib/' .. ros_package_name .. '/' .. executable_name
        end
        config.program = construct_executable_path(vim.fn.fnamemodify(vim.fn.expand('%'), ':t:r'))
        if vim.fn.executable(config.program) == 0 then
          config.program = construct_executable_path(vim.fn.input({
            prompt = 'Executable: ',
          }))
        end
        config.args = vim.split(
          vim.fn.input({
            prompt = 'Arguments: ',
          }),
          ' '
        )
        local config_copy = vim.deepcopy(config)
        setmetatable(config_copy, nil)
        config_copy.name = config_copy.name .. ' (again)'
        -- TODO: Overwrite if already exists
        table.insert(dap.configurations.cpp, 1, config_copy)
        return config
      end,
    }),
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
    {
      name = 'Attach to gdbserver :1234',
      type = 'cppdbg',
      request = 'launch',
      MIMode = 'gdb',
      miDebuggerServerAddress = 'localhost:1234',
      miDebuggerPath = '/usr/bin/gdb',
      cwd = '${workspaceFolder}',
      program = function() return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file') end,
      filterStderr = true,
      filterStdout = true,
      logging = {
        programOutput = true,
      },
    },
    {
      name = 'Attach to process',
      type = 'cppdbg',
      request = 'attach',
      program = function() return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file') end,
      processId = require('dap.utils').pick_process,
    },
  }
  dap.adapters.cppdbg = {
    type = 'executable',
    command = 'OpenDebugAD7',
    id = 'cppdbg',
  }
end

return M

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
        if ros_package_name and ros_package_workspace then
          local executable_path = ros_package_workspace
            .. '/devel/lib/'
            .. ros_package_name
            .. '/'
            .. vim.fn.fnamemodify(vim.fn.expand('%'), ':t:r')
          config.program = executable_path
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
  }
  dap.adapters.cppdbg = {
    type = 'executable',
    command = 'OpenDebugAD7',
    id = 'cppdbg',
  }
end

return M

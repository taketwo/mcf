local M = {}

function M.setup()
  local dap = require('dap')
  local install_root_dir = require('mason.settings').current.install_root_dir
  local bashdb_dir = install_root_dir .. '/packages/bash-debug-adapter/extension/bashdb_dir'
  dap.configurations.sh = {
    {
      type = 'bashdb',
      request = 'launch',
      name = 'Launch file',
      showDebugOutput = true,
      pathBashdb = bashdb_dir .. '/bashdb',
      pathBashdbLib = bashdb_dir,
      trace = true,
      file = '${file}',
      program = '${file}',
      cwd = '${workspaceFolder}',
      pathCat = 'cat',
      pathBash = '/bin/bash',
      pathMkfifo = 'mkfifo',
      pathPkill = 'pkill',
      args = {},
      env = {},
      terminalKind = 'integrated',
    },
  }
  dap.adapters.bashdb = {
    type = 'executable',
    command = 'bash-debug-adapter',
    name = 'bashdb',
  }
end

return M

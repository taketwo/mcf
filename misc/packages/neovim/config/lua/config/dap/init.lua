local M = {}

function M.setup()
  local dap = require('dap')
  dap.set_log_level('INFO') -- Helps when configuring DAP, see logs with :DapShowLog

  -- Extensions
  require('nvim-dap-virtual-text').setup({
    commented = true,
  })
  local dapui = require('dapui')
  -- TODO: Investigate what else is possible with dap-ui
  dapui.setup()
  dap.listeners.after.event_initialized['dapui_config'] = function() dapui.open() end
  dap.listeners.before.event_terminated['dapui_config'] = function() dapui.close() end
  dap.listeners.before.event_exited['dapui_config'] = function() dapui.close() end

  -- Setup debuggers
  require('config.dap.bash').setup()
  require('config.dap.cpp').setup()

  -- Keymaps
  require('config.dap.keymaps').setup()
end

return M

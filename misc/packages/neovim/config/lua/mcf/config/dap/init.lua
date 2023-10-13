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
  dap.listeners.after.event_initialized['dapui_config'] = dapui.open
  dap.listeners.after.event_initialized['keymaps'] = require('config.dap.keymaps').setup
  dap.listeners.before.event_terminated['dapui_config'] = dapui.close
  dap.listeners.before.event_exited['dapui_config'] = dapui.close
  dap.listeners.after.disconnect['dapui_config'] = dapui.close

  -- Setup debuggers
  require('mcf.config.dap.bash').setup()
  require('mcf.config.dap.cpp').setup()
end

return M

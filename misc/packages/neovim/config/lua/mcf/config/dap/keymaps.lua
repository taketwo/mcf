-- Adapted from: https://raw.githubusercontent.com/alpha2phi/neovim-for-beginner/16-dap1/lua/config/dap/keymaps.lua

local M = {}

function M.setup()
  local dap = require('dap')
  local dapui = require('dapui')
  -- local widgets = require('dap.ui.widgets')
  local wk = require('which-key')
  local keymap = {
    { '<Leader>d', group = 'Debug' },
    { '<Leader>dR', dap.run_to_cursor, desc = 'Run to cursor' },
    { '<Leader>dE', function() dapui.eval(vim.fn.input({ prompt = '[Expression] > ' })) end, desc = 'Evaluate input' },
    {
      '<Leader>dC',
      function() dap.set_breakpoint(vim.fn.input({ prompt = '[Condition] > ' })) end,
      desc = 'Conditional breakpoint',
    },
    {
      '<Leader>dL',
      function() dap.set_breakpoint(nil, nil, vim.fn.input({ prompt = '[Message] > ' })) end,
      desc = 'Logpoint',
    },
    { '<Leader>dU', dapui.toggle, desc = 'Toggle UI' },
    { '<Leader>db', dap.step_back, desc = 'Step back' },
    { '<Leader>dc', dap.continue, desc = 'Continue' },
    { '<Leader>dd', dap.disconnect, desc = 'Disconnect' },
    { '<Leader>de', dapui.eval, desc = 'Evaluate' },
    { '<Leader>dg', dap.session, desc = 'Get session' },
    -- TODO: Does not work
    -- h = { widgets.hover, 'Hover variables' },
    -- S = { widgets.scopes, 'Scopes' },
    { '<Leader>dp', dap.pause, desc = 'Pause' },
    { '<Leader>dq', dap.close, desc = 'Quit' },
    { '<Leader>dr', dap.repl.toggle, desc = 'Toggle REPL' },
    { '<Leader>dt', dap.toggle_breakpoint, desc = 'Toggle breakpoint' },
    { '<Leader>dx', dap.terminate, desc = 'Terminate' },
    { '<Leader>du', dap.step_out, desc = 'Step out' },
    { '<Leader>di', dap.step_into, desc = 'Step into' },
    { '<Leader>do', dap.step_over, desc = 'Step over' },
  }

  wk.add(keymap, {
    mode = 'n',
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = false,
  })

  local keymap_v = {
    { '<Leader>d', group = 'Debug' },
    { '<Leader>de', dapui.eval, desc = 'Evaluate' },
  }
  wk.add(keymap_v, {
    mode = 'v',
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = false,
  })

  wk.add({
    { '<F5>', dap.continue, desc = 'Continue' },
    { '<F9>', dap.toggle_breakpoint, desc = 'Toggle breakpoint' },
    { '<F10>', dap.step_over, desc = 'Step over' },
    { '<F11>', dap.step_into, desc = 'Step into' },
    -- { '<S-F11>', dap.step_out, desc = 'Step out' },
    { '<S-F5>', dap.terminate, desc = 'Stop' },
  }, {
    mode = 'n',
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = false,
  })
end

return M

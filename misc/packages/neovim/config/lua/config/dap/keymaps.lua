-- Adapted from: https://raw.githubusercontent.com/alpha2phi/neovim-for-beginner/16-dap1/lua/config/dap/keymaps.lua

local M = {}

function M.setup()
  local dap = require('dap')
  local dapui = require('dapui')
  -- local widgets = require('dap.ui.widgets')
  local wk = require('which-key')
  local keymap = {
    d = {
      name = 'Debug',
      R = { dap.run_to_cursor, 'Run to cursor' },
      E = {
        function() dapui.eval(vim.fn.input({ prompt = '[Expression] > ' })) end,
        'Evaluate input',
      },
      C = {
        function() dap.set_breakpoint(vim.fn.input({ prompt = '[Condition] > ' })) end,
        'Conditional breakpoint',
      },
      U = { dapui.toggle, 'Toggle UI' },
      b = { dap.step_back, 'Step back' },
      c = { dap.continue, 'Continue' },
      d = { dap.disconnect, 'Disconnect' },
      e = { dapui.eval, 'Evaluate' },
      g = { dap.session, 'Get session' },
      -- TODO: Does not work
      -- h = { widgets.hover, 'Hover variables' },
      -- S = { widgets.scopes, 'Scopes' },
      p = { dap.pause, 'Pause' },
      q = { dap.close, 'Quit' },
      r = { dap.repl.toggle, 'Toggle REPL' },
      t = { dap.toggle_breakpoint, 'Toggle breakpoint' },
      x = { dap.terminate, 'Terminate' },
      u = { dap.step_out, 'Step out' },
      i = { dap.step_into, 'Step into' },
      o = { dap.step_over, 'Step over' },
    },
  }

  wk.register(keymap, {
    mode = 'n',
    prefix = '<Leader>',
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = false,
  })

  local keymap_v = {
    d = {
      name = 'Debug',
      e = { dapui.eval, 'Evaluate' },
    },
  }
  wk.register(keymap_v, {
    mode = 'v',
    prefix = '<Leader>',
    buffer = nil,
    silent = true,
    noremap = true,
    nowait = false,
  })
end

return M

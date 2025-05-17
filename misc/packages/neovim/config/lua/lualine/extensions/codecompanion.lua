local ok, codecompanion = pcall(require, 'codecompanion')
if not ok then return '' end

local M = {}

M.sections = {
  lualine_a = {
    function() return 'CodeCompanion' end,
  },
  lualine_b = {
    function()
      local chat = codecompanion.buf_get_chat(vim.api.nvim_get_current_buf())
      if not chat then return nil end
      return '  ' .. chat.adapter.formatted_name
    end,
  },
  lualine_c = {
    function()
      local chat = require('codecompanion').buf_get_chat(vim.api.nvim_get_current_buf())
      if not chat then return nil end
      return chat.settings.model
    end,
  },
  lualine_x = { 'codecompanion' },
  lualine_y = { 'progress' },
  lualine_z = { 'location' },
}

M.filetypes = {
  'codecompanion',
}

return M

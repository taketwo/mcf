-- Some of the functions in this file were adapted from LazyVim
-- https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/util/init.lua

local Util = require('lazy.core.util')

local M = {}

--- Extract short plugin name from the plugin location string (e.g. 'nvim-lua/plenary.nvim' -> 'plenary')
M.extract_plugin_name = function(plugin_location)
  return plugin_location
    :match('([^/]+)$')
    :gsub('%-nvim$', '')
    :gsub('%.lua$', '')
    :gsub('%.nvim$', '')
    :gsub('%.vim$', '')
    :gsub('^vim%-', '')
    :gsub('^nvim%-', '')
end

---@param silent boolean?
---@param values? {[1]:any, [2]:any}
function M.toggle(option, silent, values)
  if values then
    if vim.opt_local[option]:get() == values[1] then
      vim.opt_local[option] = values[2]
    else
      vim.opt_local[option] = values[1]
    end
    return Util.info('Set ' .. option .. ' to ' .. vim.opt_local[option]:get(), { title = 'Option' })
  end
  vim.opt_local[option] = not vim.opt_local[option]:get()
  if not silent then
    if vim.opt_local[option]:get() then
      Util.info('Enabled ' .. option, { title = 'Option' })
    else
      Util.warn('Disabled ' .. option, { title = 'Option' })
    end
  end
end

return M

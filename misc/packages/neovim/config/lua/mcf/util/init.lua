-- Dispatches calls to corresponding LazyVim utils
-- If not found, tries to load from mcf/util

---@class mcf.util: lazyvim.util
local M = {}

setmetatable(M, {
  __index = function(t, k)
    if LazyVim[k] then return LazyVim[k] end
    ---@diagnostic disable-next-line: no-unknown
    t[k] = require('mcf.util.' .. k)
    return t[k]
  end,
})

return M

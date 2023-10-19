-- Dispatch to Lazy and LazyVim utils
-- If not found, try to load from mcf/util

local LazyUtil = require('lazyvim.util')

---@class mcf.util
---@field format lazyvim.util.format
---@field root lazyvim.util.root
---@field terminal lazyvim.util.terminal
---@field toggle lazyvim.util.toggle
local M = {}

setmetatable(M, {
  __index = function(t, k)
    if LazyUtil[k] then return LazyUtil[k] end
    ---@diagnostic disable-next-line: no-unknown
    t[k] = require('mcf.util.' .. k)
    return t[k]
  end,
})

return M

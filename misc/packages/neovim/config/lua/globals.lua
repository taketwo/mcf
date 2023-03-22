-- Source: https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/lua/tj/globals.lua

local available, pr = pcall(require, 'plenary.reload')

local reloader = require
if available then reloader = pr.reload_module end

R = function(name)
  reloader(name)
  return require(name)
end

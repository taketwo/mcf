local M = {}

--- Extract short plugin name from the plugin location string (e.g. 'nvim-lua/plenary.nvim' -> 'plenary')
M.extract_plugin_name = function(plugin_location)
  return plugin_location:match("([^/]+)$"):gsub("%-nvim$", ""):gsub("%.lua$", ""):gsub("%.nvim$", ""):gsub("%.vim$", ""):gsub("^vim%-", ""):gsub("^nvim%-", "")
end

return M

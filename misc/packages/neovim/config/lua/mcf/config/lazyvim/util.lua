-- Patch and re-export LazyVim utilities

local LazyVim = require('lazyvim.util')

-- Use format toggle generator to create two separate toggle objects
LazyVim.toggle.format_buffer = LazyVim.toggle.format(true)
LazyVim.toggle.format_global = LazyVim.toggle.format()

-- Patch toggle object names to be lowercase
for _, toggle in pairs(LazyVim.toggle) do
  if type(toggle) == 'table' and toggle.name then toggle.name = string.lower(toggle.name) end
end

return LazyVim

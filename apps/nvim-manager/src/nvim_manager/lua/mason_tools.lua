-- Mason tools management for nvim-manager
-- Self-contained Lua script for Mason operations in headless mode

local function mason_list_installed()
  local registry = require('mason-registry')

  registry.refresh(function()
    local installed_packages = registry.get_installed_packages()

    if #installed_packages == 0 then return end

    local output = {}

    for _, package in ipairs(installed_packages) do
      local version = package:get_installed_version() or 'unknown'
      output[#output + 1] = string.format('%s %s', package.name, version)
    end

    vim.print(table.concat(output, '\n'))
  end)
end

vim.api.nvim_create_user_command(
  'MasonListInstalled',
  mason_list_installed,
  { desc = 'List all installed Mason packages with their versions' }
)


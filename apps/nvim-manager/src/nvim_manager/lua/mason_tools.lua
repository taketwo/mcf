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

---Install tools with specific versions in batch
---@param tools_spec string Space-separated "tool@version" pairs
local function mason_batch_install(tools_spec)
  local tools = {}
  for tool_version in tools_spec:gmatch('%S+') do
    local tool, version = tool_version:match('([^@]+)@(.+)')
    if tool and version then
      if tools[tool] then
        vim.print(tool .. ': duplicate tool specification')
        vim.cmd('cquit 1')
        return
      end
      tools[tool] = version
    else
      vim.print(tool_version .. ': invalid tool specification')
      vim.cmd('cquit 1')
      return
    end
  end

  if next(tools) == nil then return end

  local registry = require('mason-registry')
  registry.refresh(function()
    -- First pass: verify all packages exist and find tools that need installation
    local tools_to_install = {}
    for tool, version in pairs(tools) do
      local package = registry.get_package(tool)
      if not package then
        vim.print(tool .. ': package not found in registry')
        vim.cmd('cquit 1')
        return
      end

      local current_version = package:get_installed_version()
      if current_version ~= version then
        table.insert(tools_to_install, { tool = tool, package = package, version = version })
      end
    end

    if #tools_to_install == 0 then return end

    -- Second pass: start installations for tools that need them
    for _, info in ipairs(tools_to_install) do
      info.handle = info.package:install({ version = info.version, force = true })
      info.error_output = {}
      info.handle:on('stderr', function(chunk) table.insert(info.error_output, chunk) end)
    end

    -- Wait for installations to complete
    local a = require('mason-core.async')
    local _ = require('mason-core.functional')
    a.run_blocking(function()
      a.wait_all(_.map(
        ---@param info table
        function(info)
          return function()
            a.wait(function(resolve)
              if info.handle:is_closed() then
                resolve()
              else
                info.handle:once('closed', resolve)
              end
            end)
          end
        end,
        tools_to_install
      ))
    end)

    -- Third pass: verify installations and report failures
    local has_failures = false
    for _, info in ipairs(tools_to_install) do
      local tool = info.tool
      local package = info.package
      local version = info.version

      if not package:is_installed() or package:get_installed_version() ~= version then
        local error_msg = 'installation failed'
        if info.error_output and #info.error_output > 0 then
          local captured = table.concat(info.error_output, ' '):gsub('\r?\n', ' ')
          if captured and #captured > 0 then error_msg = captured end
        end
        vim.print(tool .. ': ' .. error_msg)
        has_failures = true
      end
    end
    if has_failures then vim.cmd('cquit 1') end
  end)
end

---Uninstall tools in batch
---@param tools_spec string Space-separated tool names
local function mason_batch_uninstall(tools_spec)
  local tools = {}
  for tool in tools_spec:gmatch('%S+') do
    table.insert(tools, tool)
  end

  if next(tools) == nil then return end

  local registry = require('mason-registry')
  registry.refresh(function()
    for _, tool in ipairs(tools) do
      local package = registry.get_package(tool)
      if package and package:is_installed() then
        if not pcall(function() package:uninstall() end) then
          vim.print(tool .. ': failed to uninstall')
          vim.cmd('cquit 1')
          return
        end
      end
    end
  end)
end

vim.api.nvim_create_user_command(
  'MasonListInstalled',
  mason_list_installed,
  { desc = 'List all installed Mason packages with their versions' }
)

vim.api.nvim_create_user_command('MasonBatchInstall', function(opts) mason_batch_install(opts.args) end, {
  desc = 'Install tools with specific versions in batch',
  nargs = 1,
})

vim.api.nvim_create_user_command('MasonBatchUninstall', function(opts) mason_batch_uninstall(opts.args) end, {
  desc = 'Uninstall tools in batch',
  nargs = 1,
})

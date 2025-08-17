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
---@param tools_spec string JSON string with {tool_name: version} mappings
local function mason_batch_install(tools_spec)
  local registry = require('mason-registry')

  -- Parse JSON input
  local tools = vim.json.decode(tools_spec)
  if not tools or type(tools) ~= 'table' then
    vim.print('Error: Invalid tools specification')
    vim.cmd('cquit 1')
    return
  end

  registry.refresh(function()
    local install_handles = {}
    local install_names = {}
    local failed_installs = {}

    -- Prepare installation handles
    for tool_name, version in pairs(tools) do
      local package = registry.get_package(tool_name)
      if package then
        -- Check if already installed with correct version
        local current_version = package:get_installed_version()
        if current_version ~= version then
          vim.print(
            'Installing ' .. tool_name .. '@' .. version .. ' (current: ' .. (current_version or 'not installed') .. ')'
          )
          -- Use force flag to bypass lockfile conflicts
          local handle = package:install({ version = version, force = true })
          install_handles[#install_handles + 1] = handle
          install_names[#install_names + 1] = tool_name .. '@' .. version

          -- Capture stderr for better error reporting
          local error_output = {}
          handle:on('stderr', function(chunk) table.insert(error_output, chunk) end)

          -- Store error output for later reference
          install_handles.error_outputs = install_handles.error_outputs or {}
          install_handles.error_outputs[#install_handles] = error_output
        else
          vim.print('Skipping ' .. tool_name .. '@' .. version .. ' (already installed)')
        end
      else
        failed_installs[#failed_installs + 1] = tool_name .. ' (package not found in registry)'
      end
    end

    -- Report any package lookup failures
    if #failed_installs > 0 then
      vim.print('Failed to find packages: ' .. table.concat(failed_installs, ', '))
      vim.cmd('cquit 1')
      return
    end

    -- If nothing to install, exit successfully
    if #install_handles == 0 then
      vim.print('All tools already installed with correct versions')
      return
    end

    -- In headless mode, wait for installations to complete
    if require('mason-core.platform').is_headless then
      vim.print('Installing packages with Mason: ' .. table.concat(install_names, ', '))
      local a = require('mason-core.async')
      local _ = require('mason-core.functional')

      a.run_blocking(function()
        a.wait_all(_.map(
          ---@param handle InstallHandle
          function(handle)
            return function()
              a.wait(function(resolve)
                if handle:is_closed() then
                  resolve()
                else
                  handle:once('closed', resolve)
                end
              end)
            end
          end,
          install_handles
        ))
      end)

      -- Verify installations after all operations complete (outside run_blocking)
      local install_failures = {}
      local failure_details = {}

      local tool_index = 1
      for tool_name, version in pairs(tools) do
        local package = registry.get_package(tool_name)
        if package and package:is_installed() then
          local installed_version = package:get_installed_version()
          if installed_version ~= version then
            install_failures[#install_failures + 1] = tool_name
              .. '@'
              .. version
              .. ' (got '
              .. (installed_version or 'unknown')
              .. ')'
          end
        else
          install_failures[#install_failures + 1] = tool_name .. '@' .. version .. ' (not installed)'

          -- Include error details if available
          if install_handles.error_outputs and install_handles.error_outputs[tool_index] then
            local errors = table.concat(install_handles.error_outputs[tool_index], '')
            if errors and #errors > 0 then
              -- Extract relevant error information
              if errors:match('cargo') then
                failure_details[#failure_details + 1] = tool_name .. ': missing cargo (Rust toolchain required)'
              elseif errors:match('npm') then
                failure_details[#failure_details + 1] = tool_name .. ': missing npm (Node.js required)'
              elseif errors:match('go') then
                failure_details[#failure_details + 1] = tool_name .. ': missing go (Go toolchain required)'
              elseif errors:match('python') or errors:match('pip') then
                failure_details[#failure_details + 1] = tool_name .. ': missing python/pip'
              else
                -- Include first line of error for other cases
                local first_error_line = errors:match('([^\r\n]+)')
                if first_error_line and #first_error_line > 0 then
                  failure_details[#failure_details + 1] = tool_name .. ': ' .. first_error_line
                end
              end
            end
          end
        end
        tool_index = tool_index + 1
      end

      if #install_failures > 0 then
        vim.print('Failed to install: ' .. table.concat(install_failures, ', '))
        if #failure_details > 0 then
          vim.print('Error details:')
          for _, detail in ipairs(failure_details) do
            vim.print('  ' .. detail)
          end
        end
        vim.cmd('cquit 1')
      else
        vim.print('Successfully installed all packages')
      end
    end
  end)
end

---Uninstall tools in batch
---@param tools_spec string JSON string with array of tool names
local function mason_batch_uninstall(tools_spec)
  local registry = require('mason-registry')

  -- Parse JSON input
  local tools = vim.json.decode(tools_spec)
  if not tools or type(tools) ~= 'table' then
    vim.print('Error: Invalid tools specification')
    vim.cmd('cquit 1')
    return
  end

  registry.refresh(function()
    local uninstall_failures = {}
    local uninstalled = {}

    for _, tool_name in ipairs(tools) do
      local package = registry.get_package(tool_name)
      if package and package:is_installed() then
        local success = pcall(function() package:uninstall() end)
        if success then
          uninstalled[#uninstalled + 1] = tool_name
        else
          uninstall_failures[#uninstall_failures + 1] = tool_name
        end
      end
    end

    if #uninstall_failures > 0 then
      vim.print('Failed to uninstall: ' .. table.concat(uninstall_failures, ', '))
      vim.cmd('cquit 1')
    else
      if #uninstalled > 0 then
        vim.print('Successfully uninstalled: ' .. table.concat(uninstalled, ', '))
      else
        vim.print('No tools were uninstalled (already absent)')
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

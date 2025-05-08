-- List of tools to install with Mason
-- All configured LSP servers will be added to this list automatically and do not need to be included here explicitly
local ensure_installed = {
  'bash-debug-adapter',
  'black',
  'cpptools',
  'gersemi',
  'shellcheck',
  'shfmt',
  'stylua',
  'usort',
}

---Retrieve package names corresponding to all configured LSP servers
---@return string[] List of package names
local function get_configured_lsp_packages()
  local lsp_config_dir = vim.fn.stdpath('config') .. '/after/lsp'
  local files = require('plenary.scandir').scan_dir(lsp_config_dir, { add_dirs = false })
  local package_names = {}
  local lspconfig_to_package = require('mason-lspconfig').get_mappings().lspconfig_to_package
  for _, file in ipairs(files) do
    local server = file:match('([^/]+)%.lua$')
    if server and lspconfig_to_package[server] ~= nil then
      package_names[#package_names + 1] = lspconfig_to_package[server]
    end
  end
  return package_names
end

local function mason_ensure_installed()
  local registry = require('mason-registry')

  local packages = get_configured_lsp_packages()
  vim.list_extend(packages, ensure_installed)

  registry.refresh(function()
    local install_handles = {}
    local install_names = {}
    for _, package in ipairs(packages) do
      local p = registry.get_package(package)
      if not p:is_installed() then
        local handle = p:install()
        install_handles[#install_handles + 1] = handle
        install_names[#install_names + 1] = package
      end
    end
    if #install_handles == 0 then return end
    if require('mason-core.platform').is_headless then
      vim.print('Installing packages with Mason: ' .. table.concat(install_names, ', ') .. '\n')
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
    end
  end)
end

vim.api.nvim_create_user_command(
  'MasonEnsureInstalled',
  mason_ensure_installed,
  { desc = 'Ensure that all necessary tools and LSP servers are installed with Mason' }
)

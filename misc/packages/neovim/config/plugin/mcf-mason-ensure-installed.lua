-- List of tools to install with Mason
-- All configured LSP servers will be added to this list automatically and do not need to be included here explicitly
local ensure_installed = {
  'bash-debug-adapter',
  'black',
  'cpptools',
  'gersemi',
  'json-lsp',
  'shellcheck',
  'shfmt',
  'stylua',
  'usort',
}

local function mason_ensure_installed()
  local registry = require('mason-registry')

  local ConfigLsp = require('mcf.config.lsp')
  local packages = ConfigLsp.resolve_mason_packages(ConfigLsp.get_server_configs())
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

local overseer = require('overseer')
local priority = 40

---@type overseer.TemplateFileDefinition
local tmpl = {
  name = 'Catkin build',
  priority = priority + 1,
  tags = { overseer.TAG.BUILD },
  params = {
    args = { optional = true, type = 'list', delimiter = ' ' },
    cwd = { optional = true },
  },
  builder = function(params)
    return {
      cmd = { 'catkin' },
      args = vim.list_extend({ 'build', '--no-status' }, params.args or {}),
      cwd = params.cwd,
      components = {
        { 'on_output_parse', problem_matcher = '$gcc' },
        { 'on_result_diagnostics', remove_on_restart = true },
        'on_complete_notify',
        'on_exit_set_status',
        { 'unique', replace = false }, -- Restart existing task
      },
    }
  end,
}

---@type overseer.TemplateFileProvider
local provider = {
  condition = {
    callback = function()
      if vim.fn.executable('catkin') == 0 then return false, 'Command "catkin" not found' end
      return true
    end,
  },
  generator = function(_, cb)
    local cwd = '.'
    local templates = { overseer.wrap_template(tmpl, { desc = 'current workspace' }, { cwd = cwd }) }
    local job_id = vim.fn.jobstart({ 'catkin', 'list', '--unformatted' }, {
      cwd = cwd,
      stdout_buffered = true,
      on_stdout = vim.schedule_wrap(function(_, output)
        for _, package_name in ipairs(output) do
          if package_name ~= '' then
            local override = { name = string.format('Catkin build %s', package_name) }
            if package_name == vim.b.ros_package_name then
              override.desc = 'current package'
              override.priority = priority
            end
            table.insert(templates, overseer.wrap_template(tmpl, override, { args = { package_name }, cwd = cwd }))
          end
        end
        cb(templates)
      end),
    })
    if job_id == 0 then
      overseer.log:error("Passed invalid arguments to 'catkin'")
      cb(templates)
    elseif job_id == -1 then
      overseer.log:error("Command 'catkin' not found")
      cb(templates)
    end
  end,
}

return provider

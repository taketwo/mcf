local autoreload_timer = nil
local autoreload_enabled = false

local function start_autoreload(interval)
  if autoreload_timer then
    autoreload_timer:stop()
    autoreload_timer:close()
  end

  -- Set autoread option
  vim.opt.autoread = true

  -- Create and start timer
  autoreload_timer = vim.loop.new_timer()
  autoreload_timer:start(
    interval or 1000,
    interval or 1000,
    vim.schedule_wrap(function()
      -- Only check if we're not in command mode
      if vim.fn.mode() ~= 'c' then vim.cmd('silent! checktime') end
    end)
  )

  autoreload_enabled = true
  vim.notify('File autoreload enabled (interval: ' .. (interval or 1000) .. 'ms)', vim.log.levels.INFO)
end

local function stop_autoreload()
  if autoreload_timer then
    autoreload_timer:stop()
    autoreload_timer:close()
    autoreload_timer = nil
  end

  autoreload_enabled = false
  vim.notify('File autoreload disabled', vim.log.levels.INFO)
end

local function toggle_autoreload(interval)
  if autoreload_enabled then
    stop_autoreload()
  else
    start_autoreload(interval)
  end
end

local function handle_autoreload_command(opts)
  local args = opts.fargs
  local action = args[1] or 'toggle'
  local interval = tonumber(args[2]) or 1000

  if action == 'on' or action == 'start' then
    start_autoreload(interval)
  elseif action == 'off' or action == 'stop' then
    stop_autoreload()
  elseif action == 'toggle' then
    toggle_autoreload(interval)
  elseif action == 'status' then
    if autoreload_enabled then
      vim.notify('File autoreload is enabled', vim.log.levels.INFO)
    else
      vim.notify('File autoreload is disabled', vim.log.levels.INFO)
    end
  else
    vim.notify('Usage: :AutoReload [on|off|toggle|status] [interval_ms]', vim.log.levels.ERROR)
  end
end

vim.api.nvim_create_user_command('AutoReload', handle_autoreload_command, {
  nargs = '*',
  complete = function() return { 'on', 'off', 'toggle', 'status' } end,
  desc = 'Control file autoreload with timer for all open files',
})


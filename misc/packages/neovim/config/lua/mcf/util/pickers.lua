---@class mcf.util.pickers
local M = {}

---@class SynctankStatus
---@field linked boolean Whether the current directory is linked to a synctank workspace
---@field workspace_root? string Absolute path to the workspace root (present when linked)
---@field notes_root? string Absolute path to the notes directory (present when linked)

---@class SynctankFile
---@field path string Absolute path to the note file
---@field subdir string Subdirectory relative to notes root, empty string for root notes
---@field index integer Sequential file index
---@field slug string URL-friendly identifier
---@field kind string Document kind (e.g. spec, design, report)
---@field status string Lifecycle status (e.g. draft)
---@field name string Human-readable title
---@field date string Creation date in YYYY-MM-DD format
---@field related string[] Filenames of related notes
---@field body string File body content

---@class SynctankProject
---@field name string Project directory name
---@field path string Absolute path to the project directory in the notes store
---@field notes integer Number of note files in the project

---@class SynctankPickerOpts
---@field cwd? string Working directory for `synctank list`. Skips the link check when set.
---@field title? string Picker title. Default: Synctank
---@field on_cancel? fun() Called when the note picker is cancelled.

---@param result vim.SystemCompleted
---@return string
local function failure_detail(result)
  for _, stream in ipairs({ result.stderr, result.stdout }) do
    local text = stream and vim.trim(stream) or ''
    if text ~= '' then
      return text
    end
  end
  return 'exit ' .. tostring(result.code)
end

---@param command string
---@param result vim.SystemCompleted
local function notify_failure(command, result)
  vim.notify('synctank ' .. command .. ': ' .. failure_detail(result), vim.log.levels.ERROR)
end

---@param command string
---@param result vim.SystemCompleted
---@return any?
local function decode_json(command, result)
  if result.code ~= 0 then
    notify_failure(command, result)
    return nil
  end
  local ok, decoded = pcall(vim.json.decode, result.stdout)
  if not ok then
    vim.notify('synctank ' .. command .. ': invalid JSON', vim.log.levels.ERROR)
    return nil
  end
  return decoded
end

---@param files SynctankFile[]
---@param opts? SynctankPickerOpts
local function open_notes(files, opts)
  opts = opts or {}
  local name_col_cap = 55

  local status_width, kind_width = 0, 0
  for _, f in ipairs(files) do
    status_width = math.max(status_width, #f.status)
    kind_width = math.max(kind_width, #f.kind)
  end

  -- CLI returns oldest-first; reverse so highest index is at the bottom of the picker.
  local reversed = {}
  for i = #files, 1, -1 do
    reversed[#reversed + 1] = files[i]
  end

  local items = vim.tbl_map(
    function(f)
      return {
        text = string.format('%s %d %s %s %s', f.subdir, f.index, f.status, f.kind, f.name),
        file = f.path,
        _index = f.index,
        _name = f.name,
        _kind = f.kind,
        _status = f.status,
        _date = f.date,
        _subdir = f.subdir,
      }
    end,
    reversed
  )

  local status_hl = {
    ['draft'] = 'SynctankStatusDraft',
    ['in-progress'] = 'SynctankStatusInProgress',
    ['living'] = 'SynctankStatusLiving',
    ['complete'] = 'SynctankStatusComplete',
    ['superseded'] = 'SynctankStatusSuperseded',
  }

  local a = Snacks.picker.util.align
  ---@type snacks.picker.Config
  local picker_opts = {
    title = opts.title or 'Synctank',
    items = items,
    format = function(item)
      local hl = status_hl[item._status]
      -- Name column: optional "subdir / " prefix in Comment, then name in status hl,
      -- both sharing a capped column width so the date always fits.
      local prefix = item._subdir ~= '' and (item._subdir .. ' / ') or ''
      local name_budget = name_col_cap - #prefix
      local name_part = a(item._name, name_budget)
      return {
        { item._date .. '  ', 'Comment' },
        { a(item._status, status_width + 1), hl },
        { ' ' .. a(tostring(item._index), 4) },
        { ' ' .. a(item._kind, kind_width + 1) },
        { ' ' .. prefix, 'Comment' },
        { name_part, hl },
      }
    end,
  }

  if opts.on_cancel then
    local on_cancel = opts.on_cancel
    picker_opts.actions = {
      -- Queue the follow-up after cancel's own deferred close. Calling it
      -- directly schedules the follow-up first when leaving insert mode.
      cancel = function(picker)
        picker:norm(function()
          Snacks.picker.actions.cancel(picker)
          vim.schedule(on_cancel)
        end)
      end,
    }
  end

  Snacks.picker.pick(picker_opts)
end

---Open a picker for synctank notes.
---With no opts, lists notes linked to the current working directory and notifies
---if that directory is not a synctank workspace. With opts.cwd, lists that
---directory's notes directly.
---@param opts? SynctankPickerOpts
function M.synctank(opts)
  opts = opts or {}
  local cwd = opts.cwd
  if not cwd then
    local status_result = vim.system({ 'synctank', 'status', '--json' }, { cwd = vim.fn.getcwd() }):wait()
    ---@type SynctankStatus?
    local status = decode_json('status', status_result)
    if not status then
      return
    end
    if not status.linked then
      vim.notify('synctank: not linked in current directory', vim.log.levels.WARN)
      return
    end
    cwd = status.workspace_root
  end

  local list_result = vim.system({ 'synctank', 'list', '--json' }, { cwd = cwd }):wait()
  ---@type SynctankFile[]?
  local files = decode_json('list', list_result)
  if not files then
    return
  end
  open_notes(files, opts)
end

---Open a project picker, then the note picker for the chosen project.
---Cancel on the note picker returns to the project list.
function M.synctank_projects()
  local result = vim.system({ 'synctank', 'projects', '--json' }):wait()
  ---@type SynctankProject[]?
  local projects = decode_json('projects', result)
  if not projects then
    return
  end

  local name_width = 0
  for _, project in ipairs(projects) do
    name_width = math.max(name_width, #project.name)
  end

  local items = vim.tbl_map(function(project)
    return {
      text = project.name,
      _notes = project.notes,
      _path = project.path,
    }
  end, projects)

  local function open()
    local a = Snacks.picker.util.align
    Snacks.picker.pick({
      title = 'Synctank projects',
      items = items,
      format = function(item)
        return {
          { a(item.text, name_width + 1) },
          { tostring(item._notes), 'Comment' },
        }
      end,
      confirm = function(picker, item)
        if not item then
          return
        end
        picker:close()
        vim.schedule(function()
          M.synctank({
            cwd = item._path,
            title = item._name,
            on_cancel = open,
          })
        end)
      end,
    })
  end

  open()
end

return M

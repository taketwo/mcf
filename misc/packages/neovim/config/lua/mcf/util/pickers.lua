---@class mcf.util.pickers
local M = {}

---@class SynctankStatus
---@field linked boolean Whether the current directory is linked to a synctank workspace
---@field workspace_root? string Absolute path to the workspace root (present when linked)
---@field notes_root? string Absolute path to the notes directory (present when linked)

---@class SynctankFile
---@field path string Absolute path to the note file
---@field index integer Sequential file index
---@field slug string URL-friendly identifier
---@field kind string Document kind (e.g. spec, design, report)
---@field status string Lifecycle status (e.g. draft)
---@field name string Human-readable title
---@field date string Creation date in YYYY-MM-DD format
---@field related string[] Filenames of related notes
---@field body string File body content

---Open a snacks picker for synctank notes linked to the current working directory.
---Notifies and returns early if the cwd is not linked to a synctank workspace.
function M.synctank()
  local status_result = vim.system({ 'synctank', 'status', '--json' }, { cwd = vim.fn.getcwd() }):wait()
  ---@type SynctankStatus
  local status = vim.json.decode(status_result.stdout)

  if not status.linked then
    vim.notify('synctank: not linked in current directory', vim.log.levels.WARN)
    return
  end

  local list_result = vim.system({ 'synctank', 'list', '--json' }, { cwd = status.workspace_root }):wait()
  ---@type SynctankFile[]
  local files = vim.json.decode(list_result.stdout)

  local status_width, kind_width, name_width = 0, 0, 0
  for _, f in ipairs(files) do
    status_width = math.max(status_width, #f.status)
    kind_width = math.max(kind_width, #f.kind)
    name_width = math.max(name_width, #f.name)
  end

  local items = vim.tbl_map(
    function(f)
      return {
        text = string.format('%d %s %s %s', f.index, f.status, f.kind, f.name),
        file = f.path,
        _index = f.index,
        _name = f.name,
        _kind = f.kind,
        _status = f.status,
        _date = f.date,
      }
    end,
    files
  )

  local status_hl = {
    draft = 'SynctankStatusDraft',
    living = 'SynctankStatusLiving',
    complete = 'SynctankStatusComplete',
    superseded = 'SynctankStatusSuperseded',
  }

  local a = Snacks.picker.util.align
  Snacks.picker.pick({
    title = 'Synctank',
    items = items,
    format = function(item)
      local hl = status_hl[item._status]
      return {
        { a(item._status, status_width + 1), hl },
        { ' ' .. a(tostring(item._index), 4) },
        { ' ' .. a(item._kind, kind_width + 1) },
        { ' ' .. a(item._name, name_width), hl },
        { '  ' .. item._date, 'Comment' },
      }
    end,
  })
end

return M

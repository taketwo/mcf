local M = {}

-- Safe execution of git commands with error handling
---@param cmd string Git command to execute
---@return string? output Command output or nil on error
---@return string? error Error message if command failed
local function git_command(cmd)
  local output = vim.fn.system(cmd)
  local success = vim.v.shell_error == 0
  if not success then return nil, output end
  return vim.trim(output), nil
end

---Build a prompt for commit message generation
---@return string prompt Built prompt
M.generate_commit = function()
  local diff, diff_err = git_command('git diff --no-ext-diff --staged')
  if diff_err then
    LazyVim.error(string.format('Failed to get Git diff: %s', diff_err), { title = 'MCF/Prompts' })
    return ''
  end

  local status = git_command('git status') or ''
  local stat = git_command('git diff --staged --stat') or ''
  local branch = git_command('git branch --show-current') or ''

  local Commits = require('mcf.util.commits')
  local commit_spec = Commits.load_repo_commit_spec()
  local recent_commits_raw = require('neogit').lib.git.log.list({ '--max-count=10' })
  if not commit_spec then
    local style = 'freeform'
    for _, commit in ipairs(recent_commits_raw) do
      if Commits.parse_conventional_commit(commit.description[1]) then
        style = 'conventional'
        break
      end
    end
    commit_spec = Commits.get_commit_spec(style)
  end

  local recent_commits = {}
  for _, commit in ipairs(recent_commits_raw) do
    table.insert(recent_commits, commit.description[1])
  end
  local recent_commits_str = table.concat(recent_commits, '\n')

  local template = [[## Commit message format

%s

## Branch

%s

## Recent commits

%s

## Git status

%s

## Staged changes (summary)

%s

## Staged changes (diff)

```diff
%s
```

Based on the staged changes above, create a single git commit with an appropriate message following the format specified. Do not add an attribution footer. Do not wrap lines in the commit message body.]]

  return string.format(template, commit_spec, branch, recent_commits_str, status, stat, diff)
end

return M

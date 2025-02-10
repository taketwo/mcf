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
    LazyVim.error(string.format('Failed to get Git diff: %s', diff_err), { title = 'Code Companion' })
    return ''
  end

  local Commits = require('mcf.util.commits')
  local commit_spec = Commits.load_repo_commit_spec()
  if not commit_spec then
    local style = 'freeform'
    local commits = require('neogit').lib.git.log.list({ '--max-count=10' })
    for _, commit in ipairs(commits) do
      local conventional_commit = Commits.parse_conventional_commit(commit.description[1])
      if conventional_commit then
        style = 'conventional'
        break
      end
    end
    commit_spec = Commits.get_commit_spec(style)
  end

  local template =
    [[You are an expert at writing clear and concise git commit messages. The messages should follow the format:

%s

Given the following git diff, please generate an appropriate commit message:

```diff
%s
```

Output only the commit message without any explanations or suggestions.]]

  return string.format(template, commit_spec, diff)
end

return M

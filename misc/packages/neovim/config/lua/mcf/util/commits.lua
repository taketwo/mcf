---@class CommitMetadata
---@field types? string[] List of commit types used in the repository
---@field scopes? string[] List of scopes used in the repository
---@field branch? string Current git branch name (for JIRA style)

---@class CommitPattern
---@field name string Short identifier for the commit style
---@field description string Human-readable description
---@field format string Format written in a human-readable way
---@field pattern string Lua pattern to match this commit style
---@field example string Example commit message in this style
---@field extract_metadata fun(commits: string[]): CommitMetadata Extract style-specific metadata

local M = {}

-- Utility function to convert a set to sorted array
---@param set table<string, boolean>
---@return string[]
local function set_to_array(set)
  local arr = vim.tbl_keys(set)
  table.sort(arr)
  return arr
end

-- Safe execution of git commands with error handling
---@param cmd string The git command to execute
---@return string|nil output Command output or nil on error
---@return string|nil error Error message if command failed
local function git_command(cmd)
  local output = vim.fn.system(cmd)
  local success = vim.v.shell_error == 0

  if not success then return nil, string.format('Git command failed: %s', output) end

  return vim.trim(output), nil
end

-- Define supported commit patterns
M.PATTERNS = {
  CONVENTIONAL = {
    name = 'conventional',
    description = 'Conventional Commits specification',
    format = 'type[(scope)]: description',
    -- Simple pattern just to detect if it's a conventional commit
    pattern = '^%w+%(?[^%)]*%)?:',
    example = 'feat(parser): add array parsing support',
    extract_metadata = function(commits)
      local type_set = {}
      local scope_set = {}

      for _, commit in ipairs(commits) do
        -- Precise pattern for extracting type and scope
        local type_with_scope, scope = commit:match('^(%w+)%(([^%)]+)%):')
        if type_with_scope then
          type_set[type_with_scope] = true
          if scope and scope ~= '' then scope_set[scope] = true end
        else
          -- Try without scope
          local type = commit:match('^(%w+):')
          if type then type_set[type] = true end
        end
      end

      return {
        types = set_to_array(type_set),
        scopes = set_to_array(scope_set),
      }
    end,
  },
  MCF = {
    name = 'mcf',
    description = 'MCF style',
    format = '<Scope>: description',
    pattern = '^[%w%-]+: ',
    example = 'Core: implement new feature',
    extract_metadata = function(commits)
      local scope_set = {}
      for _, commit in ipairs(commits) do
        local scope = commit:match('^([%w%-]+): ')
        if scope then scope_set[scope] = true end
      end

      return {
        scopes = set_to_array(scope_set),
      }
    end,
  },
  CLI = {
    name = 'cli',
    description = 'CLI style',
    format = 'type/scope: description',
    pattern = '^%w+/[%w%-]+: ',
    example = 'feature/auth: add OAuth support',
    extract_metadata = function(commits)
      local type_set = {}
      local scope_set = {}

      for _, commit in ipairs(commits) do
        local type, scope = commit:match('^(%w+)/([%w%-]+): ')
        if type and scope then
          type_set[type] = true
          scope_set[scope] = true
        end
      end

      return {
        types = set_to_array(type_set),
        scopes = set_to_array(scope_set),
      }
    end,
  },
  JIRA = {
    name = 'jira',
    description = 'JIRA style',
    format = 'TICKET-1234 description',
    pattern = '^[%u]+%-%d+%s+',
    example = 'PROJ-123 Implement new feature',
    extract_metadata = function()
      local branch = vim.fn.system('git branch --show-current'):gsub('\n', '')
      return { branch = branch }
    end,
  },
  FREEFORM = {
    name = 'freeform',
    description = 'No specific format',
    format = 'Free-form text',
    pattern = '.*',
    example = 'Add new feature',
    extract_metadata = function() return {} end,
  },
}

---Gets the git diff and commit history
---@param limit? number Maximum number of commits to analyze (default: 200)
---@return string diff Git diff output or empty string
---@return string history Commit history or empty string
---@return string|nil error Error message if any command failed
local function get_git_info(limit)
  local diff, diff_err = git_command('git diff --no-ext-diff --staged')
  if diff_err then return '', '', diff_err end

  -- Use a reasonable default limit to avoid performance issues with huge repos
  limit = limit or 200
  local history, hist_err = git_command(string.format('git log --pretty=format:"%%s" -n %d', limit))
  if hist_err then return diff or '', '', hist_err end

  return diff or '', history or '', nil
end

---Formats commit style metadata for prompt
---@param metadata CommitMetadata
---@param style CommitPattern
---@return string
local function format_metadata(metadata, style)
  if not metadata then return '' end

  local parts = {}

  if metadata.types and #metadata.types > 0 then
    table.insert(parts, "Commit types used in this repository's history:")
    for _, type in ipairs(metadata.types) do
      table.insert(parts, '• ' .. type)
    end
  end

  if metadata.scopes and #metadata.scopes > 0 then
    -- Add a blank line if we already added types
    if #parts > 0 then table.insert(parts, '') end

    table.insert(parts, "Commit scopes used in this repository's history:")
    for _, scope in ipairs(metadata.scopes) do
      table.insert(parts, '• ' .. scope)
    end
  end

  if style.name == 'jira' and metadata.branch and metadata.branch ~= 'unknown' then
    -- Add a blank line if we already added other metadata
    if #parts > 0 then table.insert(parts, '') end

    table.insert(
      parts,
      string.format("Note: Your current branch name is '%s' - it may contain a JIRA ticket reference", metadata.branch)
    )
  end

  return table.concat(parts, '\n')
end

---Filter out commits that appear to be version headers
---@param commits string[] Array of commit messages
---@return string[] Filtered commit messages
local function filter_version_commits(commits)
  local filtered = {}
  -- Match common version patterns:
  -- v1.2.3, 1.2.3, 1.2.3-beta, v1.2.3-rc1, etc.
  local version_pattern = '^[v]?%d+[%.%d]*[-%.%w]*$'

  for _, commit in ipairs(commits) do
    if not commit:match(version_pattern) then table.insert(filtered, commit) end
  end
  return filtered
end

---Analyzes commit messages to determine the style and extract metadata
---@param commits string[] Array of recent commit messages
---@return CommitPattern style
---@return CommitMetadata metadata
local function analyze_commit_history(commits)
  if not commits or #commits == 0 then return M.PATTERNS.FREEFORM, {} end

  -- Filter out version commits before analysis
  commits = filter_version_commits(commits)

  -- Count matches for each style
  local best_match = {
    style = M.PATTERNS.FREEFORM,
    count = 0,
    commits = {},
  }

  for _, style in pairs(M.PATTERNS) do
    -- Skip freeform as it matches everything
    if style.name ~= 'freeform' then
      local matching_commits = {}
      local count = 0

      for _, commit in ipairs(commits) do
        if type(commit) == 'string' and commit:match(style.pattern) then
          count = count + 1
          table.insert(matching_commits, commit)
        end
      end

      -- Update best match if this style has more matches
      -- and meets the threshold (60% of commits)
      if count > best_match.count and count >= (#commits * 0.6) then
        best_match.style = style
        best_match.count = count
        best_match.commits = matching_commits
      end
    end
  end

  return best_match.style, best_match.style.extract_metadata(best_match.commits)
end

---Generates prompt based on repository's commit style
---@return string? prompt The generated prompt, nil if generation failed
---@return string? error Error message if something went wrong, nil on success
function M.generate_prompt()
  local diff, history, err = get_git_info()
  if err then return nil, string.format('Failed to get git information: %s', err) end

  local commits = vim.split(history, '\n', { plain = true, trimempty = true })
  local style, metadata = analyze_commit_history(commits)

  local template =
    [[You are an expert at writing clear and concise git commit messages. The repository you're working with uses the %s.

Format -> %s
Example -> %s

%s

When appropriate, include both a summary line and a more detailed explanation of the changes in the commit body. Separate the summary and body with a blank line. Keep the summary line focused and specific.

Given the following git diff, please generate an appropriate commit message:

```diff
%s
```

Output only the commit message without any explanations or suggestions.]]

  local metadata_str = format_metadata(metadata, style)
  return string.format(template, style.description, style.format, style.example, metadata_str, diff), nil
end

return M

local M = {}

---@class ConventionalCommit
---@field type string Commit type
---@field description string Commit description
---@field breaking_change boolean Whether this commit introduces a breaking change
---@field scope? string Commit scope
---@field body? string Commit body

---@alias CommitStyle
---| '"conventional"'
---| '"freeform"'

local CONVENTIONAL_COMMIT_SPEC = [[<type>(<optional scope>)<!>: <description>
empty separator line
<optional body>
empty separator line
<optional footer>

## Types
- API relevant changes
    - `feat` Commits, that adds or remove a new feature
    - `fix` Commits, that fixes a bug of a preceded `feat` commit
- `refactor` Commits, that rewrite/restructure your code, however does not change any API behaviour
    - `perf` Commits are special `refactor` commits, that improve performance
- `style` Commits, that do not affect the meaning (white-space, formatting, missing semi-colons, etc)
- `test` Commits, that add missing tests or correcting existing tests
- `docs` Commits, that affect documentation only
- `build` Commits, that affect build components like build tool, ci pipeline, dependencies, project version, ...
- `ops` Commits, that affect operational components like infrastructure, deployment, backup, recovery, ...
- `chore` Miscellaneous commits e.g. modifying `.gitignore`

## Scopes
The `scope` provides additional contextual information.
* Is an **optional** part of the format
* Allowed Scopes depends on the specific project
* Don't use issue identifiers as scopes

## Breaking Changes Indicator
Breaking changes should be indicated by an `!` before the `:` in the subject line e.g. `feat(api)!: remove status endpoint`
- Is an **optional** part of the format

## Description
The `description` contains a concise description of the change.
- Is a **mandatory** part of the format
- Use the imperative, present tense: "change" not "changed" nor "changes"
  - Think of `This commit will...` or `This commit should...`
- Don't capitalize the first letter
- No dot (`.`) at the end

## Body
The `body` should include the motivation for the change and contrast this with previous behavior.
- Is an **optional** part of the format
- Use the imperative, present tense: "change" not "changed" nor "changes"
- Write as normal prose that follows regular rules of punctuation and letter capitalization.
- This is the place to mention issue identifiers and their relations

## Footer
The `footer` should contain any information about **Breaking Changes**.
- Is an **optional** part of the format
- **Breaking Changes** should start with the word `BREAKING CHANGES:` followed by space or two newlines. The rest of the commit message is then used for this.]]

local FREEFORM_COMMIT_SPEC = [[<Description>
empty separator line
<Optional body>

## Description
The `description` contains a concise description of the change.
- Is a **mandatory** part of the format
- Use the imperative, present tense: "change" not "changed" nor "changes"
  - Think of `This commit will...` or `This commit should...`
- Capitalize the first letter
- No dot (`.`) at the end

## Body
The `body` should include the motivation for the change and contrast this with previous behavior.
- Is an **optional** part of the format
- Use the imperative, present tense: "change" not "changed" nor "changes"
- Write as normal prose that follows regular rules of punctuation and letter capitalization.]]

---Load commit specification for the current repository
---@return string? commit_spec Commit spec file contents or nil if not found
M.load_repo_commit_spec = function()
  local root_dir = vim.fn.system('git rev-parse --show-toplevel'):gsub('\n', '')
  local spec_file = root_dir .. '/.git-commit-spec'
  if vim.fn.filereadable(spec_file) == 1 then return table.concat(vim.fn.readfile(spec_file), '\n') end
  return nil
end

---Parse a commit message according to the Conventional Commits specification
---@param commit string Commit message to parse
---@return ConventionalCommit? conventional_commit Parsed conventional commit or nil if commit does not match the pattern
M.parse_conventional_commit = function(commit)
  local type, scope, breaking_change, description = commit:match('^(%l+)%(?([%l%-]*)%)?(!?):%s(.+)')
  if not type then return nil end
  if scope == '' then scope = nil end
  return {
    type = type,
    scope = scope,
    breaking_change = breaking_change == '!',
    description = description,
  }
end

---Get commit specification for a given commit style
---@param style CommitStyle Commit style identifier. Available styles:
---  - `"conventional"`: Follows the Conventional Commits specification (https://www.conventionalcommits.org)
---  - `"freeform"`: Simple format with description and optional body
---@return string commit_spec Commit spec for the given style
---@throws string Error message if the commit style is invalid
M.get_commit_spec = function(style)
  local COMMIT_SPECS = {
    conventional = CONVENTIONAL_COMMIT_SPEC,
    freeform = FREEFORM_COMMIT_SPEC,
  }
  return assert(COMMIT_SPECS[style], 'Unknown commit style: ' .. style)
end

return M

local new_set = MiniTest.new_set
local eq = MiniTest.expect.equality

local Commits = require('mcf.util.commits')

local T = new_set()

---@type table<string, ConventionalCommit>
local CC_MESSAGES = {
  {
    'feat: add email notifications on new direct messages',
    { type = 'feat', breaking_change = false, description = 'add email notifications on new direct messages' },
  },
  {
    'feat!: remove ticket list endpoint',
    { type = 'feat', breaking_change = true, description = 'remove ticket list endpoint' },
  },
  {
    'feat(api)!: remove status endpoint',
    { type = 'feat', scope = 'api', breaking_change = true, description = 'remove status endpoint' },
  },
  {
    'fix(cart): prevent order an empty shopping cart',
    { type = 'fix', scope = 'cart', breaking_change = false, description = 'prevent order an empty shopping cart' },
  },
  {
    'build(release): bump version to 1.0.0',
    { type = 'build', scope = 'release', breaking_change = false, description = 'bump version to 1.0.0' },
  },
  {
    'style: remove empty lines and trailing spaces',
    { type = 'style', breaking_change = false, description = 'remove empty lines and trailing spaces' },
  },
  {
    'refactor: implement fibonacci number calculation as recursion',
    { type = 'refactor', breaking_change = false, description = 'implement fibonacci number calculation as recursion' },
  },
}

---@type table<string>
local OTHER_MESSAGES = {
  { 'Neovim: add mini.test plugin' },
  { 'Lnav: fix timestamps in ROS log format' },
  { 'BAM: remove unused dependencies' },
  { 'PROJ-123: add user authentication' },
  { 'FEAT-456 update UI components' },
  { 'AD-789 fix failing tests' },
  { 'cmd/data-move: add new subcommand' },
  { 'tmux/robots: change layout of the main window' },
  { 'utils/gh-get-prs: return tuple instead of list' },
  { '6.14.24' },
  { '0.1.2' },
  { 'Enable clang-format linter' },
  { 'Bump time limits in ROS tests' },
}

---@type table<string, ConventionalCommit?>
local MESSAGES = {}
for _, v in ipairs(CC_MESSAGES) do
  table.insert(MESSAGES, v)
end
for _, v in ipairs(OTHER_MESSAGES) do
  table.insert(MESSAGES, { v[1], nil })
end

T['parse_conventional_commit()'] = new_set({ parametrize = MESSAGES })
T['parse_conventional_commit()']['parses correctly'] = function(message, commit)
  local parsed_commit = Commits.parse_conventional_commit(message)
  eq(parsed_commit, commit)
end

---@type table<CommitStyle, boolean>
local COMMIT_STYLES = {
  { 'conventional', true },
  { 'freeform', true },
  { 'invalid', false },
  { '', false },
  { 'CONVENTIONAL', false },
  { 'freestyle', false },
}

T['get_commit_spec()'] = new_set({ parametrize = COMMIT_STYLES })
T['get_commit_spec()']['validates commit styles'] = function(style, should_succeed)
  if should_succeed then
    local spec = Commits.get_commit_spec(style)
    eq(type(spec), 'string')
    eq(spec:match('^%s*$'), nil) -- spec should not be empty
  else
    eq(pcall(Commits.get_commit_spec, style), false, string.format("Expected style '%s' to fail validation", style))
  end
end

return T

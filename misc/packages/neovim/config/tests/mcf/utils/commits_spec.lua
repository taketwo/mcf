local new_set = MiniTest.new_set
local expect, eq = MiniTest.expect, MiniTest.expect.equality

local T = new_set()

-- Save as tests/commit_prompt_spec.lua
-- local T = require('mini.test')
local Commits = require('mcf.util.commits')

-- T.setup()

-- Test helper functions
local function check_style_detection(messages, expected_style)
  local detected_style = Commits.analyze_commit_history(messages)
  return detected_style.name == expected_style
end

local function get_extracted_metadata(messages)
  local style, metadata = Commits.analyze_commit_history(messages)
  return style.name, metadata
end

-- Test descriptions
describe('commit message analysis', function()
  describe('conventional commits', function()
    local test_messages = {
      'feat(api): add user endpoint',
      'fix(ui): resolve button alignment',
      'chore: update dependencies',
      'docs: update README',
      'refactor(core): simplify logic',
    }

    it('detects conventional style', function() eq(check_style_detection(test_messages, 'conventional'), true) end)

    it('extracts types correctly', function()
      local style, metadata = get_extracted_metadata(test_messages)
      T.expect(metadata.types).to.contain('feat')
      T.expect(metadata.types).to.contain('fix')
      T.expect(metadata.types).to.contain('chore')
      T.expect(metadata.types).to.contain('docs')
      T.expect(metadata.types).to.contain('refactor')
    end)

    it('extracts scopes correctly', function()
      local style, metadata = get_extracted_metadata(test_messages)
      T.expect(metadata.scopes).to.contain('api')
      T.expect(metadata.scopes).to.contain('ui')
      T.expect(metadata.scopes).to.contain('core')
    end)
  end)

  describe('jira style', function()
    local test_messages = {
      'PROJ-123 Add user authentication',
      'FEAT-456 Update UI components',
      'TEST-789 Fix failing tests',
    }

    it('detects jira style', function() T.expect(check_style_detection(test_messages, 'jira')).to_be(true) end)
  end)

  describe('mcf style', function()
    local test_messages = {
      'Core: implement feature flag system',
      'API: add rate limiting',
      'UI: redesign dashboard',
    }

    it('detects mcf style', function() T.expect(check_style_detection(test_messages, 'mcf')).to_be(true) end)

    it('extracts scopes correctly', function()
      local style, metadata = get_extracted_metadata(test_messages)
      T.expect(metadata.scopes).to.contain('Core')
      T.expect(metadata.scopes).to.contain('API')
      T.expect(metadata.scopes).to.contain('UI')
    end)
  end)

  describe('version filtering', function()
    local test_messages = {
      'v1.2.3',
      'PROJ-123 Add feature',
      '2.0.0-beta',
      'PROJ-456 Fix bug',
      'v1.2.3-rc1',
    }

    it(
      'ignores version commits in style detection',
      function() T.expect(check_style_detection(test_messages, 'jira')).to_be(true) end
    )
  end)

  describe('mixed styles', function()
    it('uses freeform when no dominant style', function()
      local mixed_messages = {
        'feat(api): add endpoint',
        'PROJ-123 Fix bug',
        'Core: update feature',
        'Just a regular commit',
      }
      T.expect(check_style_detection(mixed_messages, 'freeform')).to_be(true)
    end)
  end)
end)

-- return T

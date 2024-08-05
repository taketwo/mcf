local M = {}

-- This table maps file types to their corresponding dial groups. This is automatically populated in the config function.
-- @type table<string, string>
M.dials_by_ft = {}

---@param increment boolean
---@param g? boolean
function M.dial(increment, g)
  local mode = vim.fn.mode(true)
  -- Use visual commands for VISUAL 'v', VISUAL LINE 'V' and VISUAL BLOCK '\22'
  local is_visual = mode == 'v' or mode == 'V' or mode == '\22'
  local func = (increment and 'inc' or 'dec') .. (g and '_g' or '_') .. (is_visual and 'visual' or 'normal')
  local group = M.dials_by_ft[vim.bo.filetype] or 'default'
  return require('dial.map')[func](group)
end

return {
  {
    'monaqa/dial.nvim',
    keys = {
      {
        '<C-q>',
        function() return M.dial(true) end,
        expr = true,
        desc = 'Increment under cursor',
        mode = { 'n', 'v' },
      },
      {
        '<C-x>',
        function() return M.dial(false) end,
        expr = true,
        desc = 'Decrement under cursor',
        mode = { 'n', 'v' },
      },
      {
        'g<C-q>',
        function() return M.dial(true, true) end,
        expr = true,
        desc = 'Increment under cursor',
        mode = { 'n', 'v' },
      },
      {
        'g<C-x>',
        function() return M.dial(false, true) end,
        expr = true,
        desc = 'Decrement under cursor',
        mode = { 'n', 'v' },
      },
    },
    config = function()
      local augend = require('dial.augend')

      local logical_alias = augend.constant.new({
        elements = { '&&', '||' },
        word = false,
        cyclic = true,
      })

      local logical_word_alias = augend.constant.new({
        elements = { 'and', 'or' },
        word = true,
        cyclic = true,
      })

      local ordinal_numbers = augend.constant.new({
        elements = {
          'first',
          'second',
          'third',
          'fourth',
          'fifth',
          'sixth',
          'seventh',
          'eighth',
          'ninth',
          'tenth',
        },
        word = false,
        cyclic = true,
      })

      local weekdays = augend.constant.new({
        elements = {
          'Monday',
          'Tuesday',
          'Wednesday',
          'Thursday',
          'Friday',
          'Saturday',
          'Sunday',
        },
        word = true,
        cyclic = true,
      })

      local months = augend.constant.new({
        elements = {
          'January',
          'February',
          'March',
          'April',
          'May',
          'June',
          'July',
          'August',
          'September',
          'October',
          'November',
          'December',
        },
        word = true,
        cyclic = true,
      })

      local capitalized_boolean = augend.constant.new({
        elements = {
          'True',
          'False',
        },
        word = true,
        cyclic = true,
      })

      local groups = {
        default = {
          augend.integer.alias.decimal, -- nonnegative decimal number (0, 1, 2, 3, ...)
          augend.integer.alias.hex, -- nonnegative hex number  (0x01, 0x1a1f, etc.)
          augend.date.alias['%Y/%m/%d'],
          augend.date.alias['%Y-%m-%d'],
          augend.semver.alias.semver,
          ordinal_numbers,
          weekdays,
          months,
        },
        cpp = {
          augend.constant.alias.bool,
          logical_alias,
        },
        lua = {
          augend.constant.alias.bool,
          logical_word_alias,
        },
        markdown = {
          augend.misc.alias.markdown_header,
        },
        python = {
          capitalized_boolean,
          logical_word_alias,
        },
        yaml = {
          augend.constant.alias.bool,
        },
      }

      for group_name, group in pairs(groups) do
        if group_name ~= 'default' then
          groups[group_name] = vim.tbl_extend('force', vim.deepcopy(groups.default), group)
          M.dials_by_ft[group_name] = group_name
        end
      end

      require('dial.config').augends:register_group(groups)
    end,
  },
}

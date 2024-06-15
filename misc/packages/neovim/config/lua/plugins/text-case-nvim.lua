return {
  {
    'johmsalas/text-case.nvim',
    keys = function()
      local T = {
        { '.', 'to_dot_case', 'dot.case' },
        { '/', 'to_path_case', 'path/case' },
        { 'c', 'to_camel_case', 'camelCase' },
        { 'd', 'to_dash_case', 'dash-case' },
        { 'l', 'to_lower_case', 'lower case' },
        { 'C', 'to_constant_case', 'CONSTANT_CASE' },
        { 'p', 'to_pascal_case', 'PascalCase' },
        { 's', 'to_snake_case', 'snake_case' },
        { 't', 'to_title_case', 'Title Case' },
        { 'u', 'to_upper_case', 'UPPER CASE' },
      }
      local keys = {
        { '<Leader>c', '', desc = 'Change case', mode = { 'n', 'v' } },
        { '<Leader>co', '', desc = 'Operator' },
        { '<Leader>cr', '', desc = 'Rename with LSP' },
      }
      for _, entry in ipairs(T) do
        table.insert(keys, {
          ('<Leader>c' .. entry[1]),
          function() require('textcase').quick_replace(entry[2]) end,
          mode = { 'n', 'v' },
          desc = entry[3],
        })
        table.insert(keys, {
          ('<Leader>co' .. entry[1]),
          function() require('textcase').operator(entry[2]) end,
          desc = entry[3],
        })
        table.insert(keys, {
          ('<Leader>cr' .. entry[1]),
          function() require('textcase').lsp_rename(entry[2]) end,
          desc = entry[3],
        })
      end
      return keys
    end,
  },
}

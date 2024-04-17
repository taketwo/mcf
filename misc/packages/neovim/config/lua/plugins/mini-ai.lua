return {
  {
    'echasnovski/mini.ai',
    event = 'VeryLazy',
    dependencies = { 'nvim-treesitter-textobjects' },
    opts = function()
      local ai = require('mini.ai')
      return {
        n_lines = 500,
        custom_textobjects = {
          A = ai.gen_spec.treesitter({ a = '@assignment.outer', i = '@assignment.inner' }, {}),
          o = ai.gen_spec.treesitter({
            a = { '@block.outer', '@conditional.outer', '@loop.outer' },
            i = { '@block.inner', '@conditional.inner', '@loop.inner' },
          }, {}),
          f = ai.gen_spec.treesitter({ a = '@function.outer', i = '@function.inner' }, {}),
          F = { '```[^\n]*\n().-()\n```' },
          c = ai.gen_spec.treesitter({ a = '@class.outer', i = '@class.inner' }, {}),
          C = ai.gen_spec.treesitter({ a = '@comment.outer', i = '@comment.inner' }, {}),
          d = { '%f[%d]%d+' },
        },
      }
    end,
    config = function(_, opts)
      require('mini.ai').setup(opts)
      local i = {
        [' '] = 'Whitespace',
        ['"'] = 'Balanced "',
        ["'"] = "Balanced '",
        ['`'] = 'Balanced `',
        ['('] = 'Balanced (',
        [')'] = 'Balanced ) including white-space',
        ['>'] = 'Balanced > including white-space',
        ['<lt>'] = 'Balanced <',
        [']'] = 'Balanced ] including white-space',
        ['['] = 'Balanced [',
        ['}'] = 'Balanced } including white-space',
        ['{'] = 'Balanced {',
        ['?'] = 'User prompt',
        _ = 'Underscore',
        a = 'Argument',
        A = 'Assignment',
        b = 'Balanced ), ], }',
        c = 'Class',
        C = 'Comment',
        d = 'Digits',
        f = 'Function',
        F = 'Fenced code block',
        o = 'Block, conditional, loop',
        q = 'Quote `, ", \'',
        t = 'Tag',
      }
      local a = vim.deepcopy(i)
      for k, v in pairs(a) do
        a[k] = v:gsub(' including.*', '')
      end

      local ic = vim.deepcopy(i)
      local ac = vim.deepcopy(a)
      for key, name in pairs({ n = 'next', l = 'last' }) do
        i[key] = vim.tbl_extend('force', { name = 'Inside ' .. name .. ' textobject' }, ic)
        a[key] = vim.tbl_extend('force', { name = 'Around ' .. name .. ' textobject' }, ac)
      end
      require('which-key').register({
        mode = { 'o', 'x' },
        i = i,
        a = a,
      })
    end,
  },
}

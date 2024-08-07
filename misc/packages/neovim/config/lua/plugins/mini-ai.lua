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
      LazyVim.on_load('which-key.nvim', function()
        vim.schedule(function()
          -- NOTE: Key setup below is based on LazyVim.mini.ai_whichkey(). The objects table was
          -- modified to match our objects, the rest was kept the same.
          local objects = {
            { ' ', desc = 'Whitespace' },
            { '"', desc = 'Balanced "' },
            { "'", desc = "Balanced '" },
            { '(', desc = 'Balanced (' },
            { ')', desc = 'Balanced ) including white-space' },
            { '<', desc = 'Balanced < block' },
            { '>', desc = 'Balanced > including white-space' },
            { '?', desc = 'User prompt' },
            { '[', desc = 'Balanced [' },
            { ']', desc = 'Balanced ] including white-space' },
            { '_', desc = 'Underscore' },
            { '`', desc = 'Balanced `' },
            { 'a', desc = 'Argument' },
            { 'A', desc = 'Assignment' },
            { 'b', desc = 'Balanced ), ], }' },
            { 'c', desc = 'Class' },
            { 'C', desc = 'Comment' },
            { 'd', desc = 'Digit(s)' },
            { 'f', desc = 'Function' },
            { 'F', desc = 'Fenced code block' },
            { 'o', desc = 'Block, conditional, loop' },
            { 'q', desc = 'Balanced `, ", \'' },
            { 't', desc = 'Tag' },
            { 'i', desc = 'Indent' },
            { '{', desc = 'Balanced {' },
            { '}', desc = 'Balanced } including white-space' },
          }
          local ret = { mode = { 'o', 'x' } }
          ---@type table<string, string>
          local mappings = vim.tbl_extend('force', {}, {
            around = 'a',
            inside = 'i',
            around_next = 'an',
            inside_next = 'in',
            around_last = 'al',
            inside_last = 'il',
          }, opts.mappings or {})
          mappings.goto_left = nil
          mappings.goto_right = nil
          for name, prefix in pairs(mappings) do
            name = name:gsub('^around_', ''):gsub('^inside_', '')
            ret[#ret + 1] = { prefix, group = name }
            for _, obj in ipairs(objects) do
              local desc = obj.desc
              if prefix:sub(1, 1) == 'i' then desc = desc:gsub(' with ws', '') end
              ret[#ret + 1] = { prefix .. obj[1], desc = obj.desc }
            end
          end
          require('which-key').add(ret, { notify = false })
        end)
      end)
    end,
  },
}

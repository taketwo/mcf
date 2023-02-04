return {
  {
    'kylechui/nvim-surround',
    event = 'BufWinEnter',
    opts = {
      keymaps = {
        insert = '<C-g>s',
        insert_line = '<C-g>S',
        normal = 'ys',
        normal_cur = 'yss',
        normal_line = 'yS',
        normal_cur_line = 'ySS',
        visual = 'S',
        visual_line = 'gS',
        delete = 'ds',
        change = 'ks',
      },
      surrounds = {
        -- By default, using the opening brackets leads to spaces being added around the object.
        -- This overrides the surround pairs to avoid that behavior.
        ['('] = { add = { '(', ')' } },
        ['['] = { add = { '[', ']' } },
        ['{'] = { add = { '{', '}' } },
        ['<'] = { add = { '<', '>' } },
      },
    },
  },
}

return {
  {
    'kylechui/nvim-surround',
    event = 'BufWinEnter',
    keys = {
      -- Same as defaults
      { 'ys', '<Plug>(nvim-surround-normal)', mode = { 'n' }, desc = 'Add a surrounding pair around a motion' },
      {
        'yss',
        '<Plug>(nvim-surround-normal-cur)',
        mode = { 'n' },
        desc = 'Add a surrounding pair around the current line',
      },
      {
        'yS',
        '<Plug>(nvim-surround-normal-line)',
        mode = { 'n' },
        desc = 'Add a surrounding pair around a motion, on new lines',
      },
      {
        'ySS',
        '<Plug>(nvim-surround-normal-cur-line)',
        mode = { 'n' },
        desc = 'Add a surrounding pair around the current line, on new lines',
      },
      { 'ds', '<Plug>(nvim-surround-delete)', mode = { 'n' }, desc = 'Delete a surrounding pair' },
      -- k instead of c to avoid conflicts with our mappings
      { 'ks', '<Plug>(nvim-surround-change)', mode = { 'n' }, desc = 'Change a surrounding pair' },
      {
        'kS',
        '<Plug>(nvim-surround-change-line)',
        mode = { 'n' },
        desc = 'Change a surrounding pair, putting replacements on new lines',
      },
    },
    opts = {
      surrounds = {
        -- By default, using the opening brackets leads to spaces being added around the object.
        -- This overrides the surround pairs to avoid that behavior.
        ['('] = { add = { '(', ')' } },
        ['['] = { add = { '[', ']' } },
        ['{'] = { add = { '{', '}' } },
        ['<'] = { add = { '<', '>' } },
      },
    },
    init = function()
      -- The plugin provides default mappings that are mostly fine except to cs/cS in normal mode. There is no way to disable only
      -- some of normal mode mappings, so we have to disable all of them and re-create with the keys we want.
      vim.g.nvim_surround_no_normal_mappings = true
    end,
  },
}

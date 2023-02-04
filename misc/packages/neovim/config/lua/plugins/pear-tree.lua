return {
  {
    'tmsvg/pear-tree',
    event = 'InsertEnter',
    keys = {
      { '<BS>', '<Plug>(PearTreeBackspace)', mode = 'i' },
      { '<C-s>', '<Plug>(PearTreeJump)', mode = 'i' },
    },
    init = function()
      vim.g.pear_tree_repeatable_expand = 0
      vim.g.pear_tree_map_special_keys = 0
    end,
  },
}

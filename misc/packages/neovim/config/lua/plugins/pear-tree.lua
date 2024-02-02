return {
  {
    'tmsvg/pear-tree',
    event = 'InsertEnter',
    keys = {
      { '<BS>', '<Plug>(PearTreeBackspace)', mode = 'i', desc = 'Delete character before cursor' },
      { '<CR>', '<Plug>(PearTreeExpand)', mode = 'i', desc = 'Begin new line' },
      { '<Space>', '<Plug>(PearTreeSpace)', mode = 'i', desc = 'Insert space' },
      { '<C-s>', '<Plug>(PearTreeJump)', mode = 'i', desc = 'Jump past closing pair' },
    },
    init = function()
      vim.g.pear_tree_repeatable_expand = 0
      vim.g.pear_tree_map_special_keys = 0
    end,
  },
}

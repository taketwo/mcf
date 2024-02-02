return {
  {
    'tmsvg/pear-tree',
    -- NOTE: This plugin ships with a number of ftplugins that customize auto-pairing behavior for
    -- specific filetypes. We need to disable lazy loading to ensure that the ftplugins are loaded.
    lazy = false,
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

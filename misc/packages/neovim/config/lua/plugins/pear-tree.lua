return {
  {
    'tmsvg/pear-tree',
    -- NOTE: This plugin ships with a number of ftplugins that customize auto-pairing behavior for
    -- specific filetypes. We need to disable lazy loading to ensure that the ftplugins are loaded.
    lazy = false,
    keys = function()
      local keys = {
        { '<BS>', '<Plug>(PearTreeBackspace)', desc = 'Delete character before cursor' },
        { '<CR>', '<Plug>(PearTreeExpand)', desc = 'Begin new line' },
        { '<Space>', '<Plug>(PearTreeSpace)', desc = 'Insert space' },
        { '<C-s>', '<Plug>(PearTreeJump)', desc = 'Jump past closing pair' },
      }
      for _, key in ipairs(keys) do
        key.mode = 'i'
        key.ft = '!TelescopePrompt'
      end
      return keys
    end,
    init = function()
      vim.g.pear_tree_ft_disabled = { 'TelescopePrompt' }
      vim.g.pear_tree_repeatable_expand = 0
      vim.g.pear_tree_map_special_keys = 0
      vim.g.pear_tree_smart_openers = 1
      vim.g.pear_tree_smart_closers = 1
      vim.g.pear_tree_smart_backspace = 1
    end,
  },
}

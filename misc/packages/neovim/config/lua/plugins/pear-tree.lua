return {
  {
    'tmsvg/pear-tree',
    -- NOTE: This plugin ships with a number of ftplugins that customize auto-pairing behavior for
    -- specific filetypes. We need to disable lazy loading to ensure that the ftplugins are loaded.
    lazy = false,
    keys = function()
      -- We need to map special keys to <Plug>(PearTreeXXX) mappings. The difficulty is that in
      -- the TelescopePrompt buffers, such mappings will not be available. As a workaround, we
      -- let the keys call the underlying plugin functions directly, without going through the
      -- <Plug>(PearTreeXXX) mappings. The carrespondence between the mappings and the plugin
      -- functions is based on the PearTree source code:
      --     https://github.com/tmsvg/pear-tree/blob/3bb209d9637d6bd7506040b2fcd158c9a7917db3/plugin/pear-tree.vim#L132-L138
      local _keys = {
        { '<BS>', 'Backspace', 'Delete character before cursor' },
        { '<CR>', 'PrepareExpansion', 'Begin new line' },
        { '<Space>', 'Space', 'Insert space' },
        { '<C-s>', 'JumpOut', 'Jump past closing pair' },
      }
      local keys = {}
      for _, key in ipairs(_keys) do
        table.insert(keys, {
          key[1],
          function() return vim.fn['pear_tree#insert_mode#' .. key[2]]() end,
          desc = key[3],
          mode = 'i',
          expr = true,
          replace_keycodes = false,
          silent = true,
        })
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

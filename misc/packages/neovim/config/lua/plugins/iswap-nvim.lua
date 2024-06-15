return {
  {
    'mizlan/iswap.nvim',
    dependencies = {
      -- We use Leap highlight groups
      'ggandor/leap.nvim',
    },
    cmd = {
      'ISwap',
      'ISwapWith',
      'ISwapWithLeft',
      'ISwapWithRight',
      'ISwapNode',
      'ISwapNodeWith',
      'ISwapNodeWithLeft',
      'ISwapNodeWithRight',
      'IMove',
      'IMoveWith',
      'IMoveWithLeft',
      'IMoveWithRight',
      'IMoveNode',
      'IMoveNodeWith',
      'IMoveNodeWithLeft',
      'IMoveNodeWithRight',
    },
    keys = {
      { '<Leader>s', '', desc = 'Shift/swap object' },
      { '<Leader>sw', '<cmd>ISwapWith<cr>', desc = 'Swap with' },
      { '<Leader>sW', '<cmd>ISwapNodeWith<cr>', desc = 'Swap node with' },
    },
    opts = {
      -- The keys that will be used as a selection, in order
      keys = 'aoeuhtnsi',
      -- Highlight group for the sniping value (asdf etc.)
      hl_snipe = 'LeapLabelPrimary',
      -- Disable post-operation highlight flashing
      flash_style = false,
    },
    config = function(_, opts)
      require('leap').init_highlight(true)
      require('iswap').setup(opts)
    end,
  },
}

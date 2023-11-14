return {
  {
    'taketwo/barbar.nvim',
    branch = 'schedule-wrap',
    event = 'VeryLazy',
    dependencies = {
      { 'nvim-tree/nvim-web-devicons' },
      {
        'folke/which-key.nvim',
        opts = {
          defaults = {
            ['<Leader>b'] = {
              name = 'Buffer',
              c = { name = 'Close' },
              s = { name = 'Sort' },
            },
          },
        },
      },
    },
    keys = {
      { 'H', '<cmd>BufferPrevious<cr>', desc = 'Previous buffer' },
      { 'N', '<cmd>BufferNext<cr>', desc = 'Next buffer' },
      { '<F11>', '<cmd>BufferClose<cr>', desc = 'Close buffer' },
      { '<Leader>bcc', '<cmd>BufferCloseAllButCurrent<cr>', desc = 'Close all but current' },
      { '<Leader>bcl', '<cmd>BufferCloseBuffersLeft<cr>', desc = 'Close all on the left' },
      { '<Leader>bcr', '<cmd>BufferCloseBuffersRight<cr>', desc = 'Close all on the right' },
      { '<Leader>bcv', '<cmd>BufferCloseAllButVisible<cr>', desc = 'Close all but visible' },
      { '<Leader>bh', '<cmd>BufferMovePrevious<cr>', desc = 'Move buffer left' },
      { '<Leader>bn', '<cmd>BufferMoveNext<cr>', desc = 'Move buffer right' },
      { '<Leader>bp', '<cmd>BufferPin<cr>', desc = 'Pin buffer' },
      { '<Leader>bsd', '<cmd>BufferOrderByDirectory<cr>', desc = 'Sort buffers by directory' },
      { '<Leader>bsl', '<cmd>BufferOrderByLanguage<cr>', desc = 'Sort buffers by language' },
      { '<Leader>bsn', '<cmd>BufferOrderByBufferNumber<cr>', desc = 'Sort buffers by buffer number' },
      { '<Leader>b<Space>', '<cmd>BufferPick<cr>', desc = 'Jump to buffer' },
    },
    opts = {
      icons = {
        button = false,
        modified = {
          button = false,
        },
      },
    },
  },
}

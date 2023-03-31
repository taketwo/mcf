return {
  {
    'romgrk/barbar.nvim',
    event = 'VeryLazy',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    opts = {
      icons = {
        button = false,
        modified = {
          button = false,
        },
      },
    },
    config = function(_, opts)
      require('barbar').setup(opts)
      -- TODO: Migrate to 'keys' option
      require('which-key').register({
        H = { '<cmd>BufferPrevious<cr>', 'Previous buffer' },
        N = { '<cmd>BufferNext<cr>', 'Next buffer' },
        ['<F11>'] = { '<cmd>BufferClose<cr>', 'Close buffer' },
        ['<Leader>'] = {
          b = {
            name = 'Buffer',
            c = {
              name = 'Close',
              c = { '<cmd>BufferCloseAllButCurrent<cr>', 'Close all but current' },
              l = { '<cmd>BufferCloseBuffersLeft<cr>', 'Close all on the left' },
              r = { '<cmd>BufferCloseBuffersRight<cr>', 'Close all on the right' },
              v = { '<cmd>BufferCloseAllButVisible<cr>', 'Close all but visible' },
            },
            h = { '<cmd>BufferMovePrevious<cr>', 'Move buffer left' },
            n = { '<cmd>BufferMoveNext<cr>', 'Move buffer right' },
            p = { '<cmd>BufferPin<cr>', 'Pin buffer' },
            s = {
              name = 'Sort',
              d = { '<cmd>BufferOrderByDirectory<cr>', 'Sort buffers by directory' },
              l = { '<cmd>BufferOrderByLanguage<cr>', 'Sort buffers by language' },
              n = { '<cmd>BufferOrderByBufferNumber<cr>', 'Sort buffers by buffer number' },
            },
            ['<Space>'] = { '<cmd>BufferPick<cr>', 'Jump to buffer' },
          },
        },
      })
    end,
  },
}

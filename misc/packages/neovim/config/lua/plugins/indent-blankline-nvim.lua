return {
  {
    'lukas-reineke/indent-blankline.nvim',
    event = { 'BufReadPost', 'BufNewFile' },
    opts = {
      indent = {
        char = '│',
        tab_char = '│',
        highlight = 'IblIndent',
      },
      whitespace = { highlight = { 'Whitespace', 'NonText' } },
      scope = { enabled = false },
      exclude = {
        filetypes = {
          'help',
          'alpha',
          'dashboard',
          'neo-tree',
          'Trouble',
          'lazy',
          'mason',
          'notify',
          'toggleterm',
          'lazyterm',
        },
      },
    },
    config = function(_, opts)
      require('ibl').setup(opts)
      LazyVim.toggle.map('<Leader>ug', {
        name = 'indentation guides',
        get = function() return require('ibl.config').get_config(0).enabled end,
        set = function(state) require('ibl').setup_buffer(0, { enabled = state }) end,
      })
    end,
  },
}

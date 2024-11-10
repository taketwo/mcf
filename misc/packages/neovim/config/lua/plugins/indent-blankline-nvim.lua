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
          'Trouble',
          'alpha',
          'dashboard',
          'help',
          'lazy',
          'mason',
          'neo-tree',
          'notify',
          'snacks_terminal',
        },
      },
    },
    config = function(_, opts)
      require('ibl').setup(opts)
      Snacks.toggle({
        name = 'indentation guides',
        get = function() return require('ibl.config').get_config(0).enabled end,
        set = function(state) require('ibl').setup_buffer(0, { enabled = state }) end,
      }):map('<Leader>ug')
    end,
  },
}

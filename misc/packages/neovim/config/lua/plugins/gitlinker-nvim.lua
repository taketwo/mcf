return {
  {
    'ruifm/gitlinker.nvim',
    dependencies = {
      { 'nvim-lua/plenary.nvim' },
    },
    keys = {
      { '<Leader>g', '', desc = 'Git', mode = { 'n', 'x' } },
      { '<Leader>gu', desc = 'Copy file URL' },
      { '<Leader>gu', mode = 'x', desc = 'Copy line(s) URL' },
      { '<Leader>gU', function() require('gitlinker').get_repo_url() end, desc = 'Copy repository URL' },
    },
    opts = {
      opts = {
        add_current_line_on_normal_mode = false,
      },
      mappings = '<Leader>gu',
    },
  },
}

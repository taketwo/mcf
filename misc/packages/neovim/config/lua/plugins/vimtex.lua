return {
  {
    'lervag/vimtex',
    ft = { 'tex' },
    init = function()
      vim.g.tex_flavor = 'latex'
      vim.g.vimtex_mappings_enabled = 0
      vim.g.vimtex_compiler_enabled = 1
      vim.g.vimtex_compiler_method = 'tectonic'
      vim.g.vimtex_view_method = 'zathura'
      vim.g.vimtex_quickfix_enabled = 0
    end,
    -- TODO: Plugin has a bunch of useful commands that can be mapped
  },
}

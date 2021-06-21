if has('nvim')
    lua <<EOF
require'nvim-treesitter.configs'.setup {
  ensure_installed = { "bash", "c", "cpp", "python" }, -- one of "all", "maintained", or a list of languages
  highlight = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "=",
      node_incremental = "=",
      scope_incremental = "/",
      node_decremental = "+",
    },
  },
  indent = {
    enable = false,
  }
}
EOF
endif

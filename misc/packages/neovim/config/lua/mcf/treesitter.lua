local treesitter_configs = require('nvim-treesitter.configs')

treesitter_configs.setup {
  ensure_installed = { -- one of "all", "maintained", or a list of languages
      "bash",
      "c",
      "cmake",
      "comment",
      "cpp",
      "json",
      "make",
   -- "markdown",    seems to be broken
      "nix",
      "python",
      "toml",
      "vim"
  },
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
  },
  playground = {
    enable = true,
  }
}

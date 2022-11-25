local actions = require('telescope.actions')
local telescope = require('telescope')

telescope.setup{
  defaults = {
    mappings = {
      i = {
        ["<A-t>"] = actions.move_selection_next,
        ["<A-c>"] = actions.move_selection_previous,
        ["<A-n>"] = actions.preview_scrolling_down,
        ["<A-h>"] = actions.preview_scrolling_up,
        ["<Esc>"] = actions.close,
      },
    }
  },
  pickers = {
    find_files = {
      find_command = {"rg", "--ignore", "--hidden", "--files"}
    }
  },
  extensions = {
    ["fzf"] = {},
    ["ui-select"] = {
      require("telescope.themes").get_dropdown {
        -- even more opts
      }

      -- pseudo code / specification for writing custom displays, like the one
      -- for "codeactions"
      -- specific_opts = {
      --   [kind] = {
      --     make_indexed = function(items) -> indexed_items, width,
      --     make_displayer = function(widths) -> displayer
      --     make_display = function(displayer) -> function(e)
      --     make_ordinal = function(e) -> string
      --   },
      --   -- for example to disable the custom builtin "codeactions" display
      --      do the following
      --   codeactions = false,
      -- }
    }
  }
}

telescope.load_extension('fzf')
telescope.load_extension('ui-select')
telescope.load_extension('file_browser')

local opts = { noremap=true, silent=true }
vim.api.nvim_set_keymap("n", "<Leader>..", ":Telescope find_files<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>.b", ":Telescope buffers<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>.g", ":Telescope live_grep<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>.s", ":Telescope lsp_document_symbols<CR>", opts)
vim.api.nvim_set_keymap("n", "<Leader>.f", ":Telescope file_browser<CR>", opts)
vim.api.nvim_set_keymap("n", "<Space>", ":Telescope buffers<CR>", opts)
vim.api.nvim_set_keymap("n", "z=", ":Telescope spell_suggest<CR>", opts)

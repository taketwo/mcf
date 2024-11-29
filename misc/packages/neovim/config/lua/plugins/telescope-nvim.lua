return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-lua/popup.nvim',
      { 'nvim-telescope/telescope-fzf-native.nvim', build = 'make' },
      'nvim-telescope/telescope-symbols.nvim',
    },
    cmd = { 'Telescope' },
    keys = {
      { '<Leader>.<Space>', '<cmd>Telescope resume<cr>', desc = 'Resume previous search' },
      { '<Leader>..', '<cmd>Telescope find_files<cr>', desc = 'Open file' },
      { '<Leader>.;', '<cmd>Telescope command_history<cr>', desc = 'Command history' },
      { '<Leader>.b', '<cmd>Telescope buffers sort_mru=true sort_lastused=true<cr>', desc = 'Jump to buffer' },
      { '<Leader>.c', '<cmd>Telescope commands<cr>', desc = 'Commands' },
      { '<Leader>.g', '<cmd>Telescope live_grep<cr>', desc = 'Live grep' },
      { '<Leader>.h', '<cmd>Telescope help_tags<cr>', desc = 'Open help' },
      { '<Leader>.k', '<cmd>Telescope keymaps<cr>', desc = 'Keymaps' },
      { '<Leader>.l', '<cmd>Telescope loclist<cr>', desc = 'Location list' },
      { '<Leader>.m', '<cmd>Telescope marks<cr>', desc = 'Jump to mark' },
      { '<Leader>.q', '<cmd>Telescope quickfix<cr>', desc = 'Quickfix list' },
      { '<Leader>.r', '<cmd>Telescope registers<cr>', desc = 'Registers' },
      -- TODO: Consider moving next three keymaps to lspconfig.lua and changing prefix
      { '<Leader>.s', '<cmd>Telescope lsp_document_symbols<cr>', desc = 'Jump to symbol' },
      { '<Leader>.S', '<cmd>Telescope lsp_dynamic_workspace_symbols<cr>', desc = 'Jump to symbol (workspace)' },
      { '<Leader>lr', '<cmd>Telescope lsp_references<cr>', desc = 'References' },
      { '<Leader>gC', '<cmd>Telescope git_branches<cr>', desc = 'Check out branch' },
      { '<Leader>gl', '<cmd>Telescope git_bcommits<cr>', desc = 'Log (buffer only)' },
      { '<Leader>gL', '<cmd>Telescope git_commits<cr>', desc = 'Log (everything)' },
      {
        '<space>',
        '<cmd>Telescope buffers show_all_buffers=true sort_mru=true sort_lastused=true<cr>',
        desc = 'Jump to buffer',
      },
      { 'z=', '<cmd>Telescope spell_suggest<cr>', desc = 'Spelling suggestions' },
    },
    opts = {
      defaults = {
        prompt_prefix = ' ',
        selection_caret = ' ',
        mappings = {
          i = {
            ['<A-t>'] = function(...) return require('telescope.actions').move_selection_next(...) end,
            ['<A-c>'] = function(...) return require('telescope.actions').move_selection_previous(...) end,
            ['<A-n>'] = function(...) return require('telescope.actions').preview_scrolling_down(...) end,
            ['<A-h>'] = function(...) return require('telescope.actions').preview_scrolling_up(...) end,
            ['<A-]>'] = function(...) return require('telescope.actions').cycle_previewers_next(...) end,
            ['<A-[>'] = function(...) return require('telescope.actions').cycle_previewers_prev(...) end,
            ['<Esc>'] = function(...) return require('telescope.actions').close(...) end,
            ['<C-t>'] = function(...) return require('trouble.sources.telescope').open(...) end,
            -- TODO: Consider adding keymaps similar to the following
            -- ['<a-i>'] = function() Util.telescope('find_files', { no_ignore = true })() end,
            -- ['<a-h>'] = function() Util.telescope('find_files', { hidden = true })() end,
          },
          n = {
            ['<C-t>'] = function(...) return require('trouble.sources.telescope').open(...) end,
          },
        },
        -- Open files in the first window that is an actual file.
        -- Use the current window if no other window is available.
        get_selection_window = function()
          local wins = vim.api.nvim_list_wins()
          table.insert(wins, 1, vim.api.nvim_get_current_win())
          for _, win in ipairs(wins) do
            local buf = vim.api.nvim_win_get_buf(win)
            if vim.bo[buf].buftype == '' then return win end
          end
          return 0
        end,
      },
      pickers = {
        find_files = {
          -- Include hidden files, but exclude `.git/` and its contents
          find_command = { 'rg', '--files', '--hidden', '--glob', '!**/.git/*' },
        },
      },
      extensions = {
        ['fzf'] = {},
      },
    },
    config = function(_, opts)
      require('telescope').setup(opts)
      require('telescope').load_extension('fzf')
    end,
  },
}

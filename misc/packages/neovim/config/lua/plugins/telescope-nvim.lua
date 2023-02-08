return {
  {
    'nvim-telescope/telescope.nvim',
    dependencies = {
      'nvim-lua/plenary.nvim',
      'nvim-lua/popup.nvim',
      {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
      'nvim-telescope/telescope-symbols.nvim',
      'nvim-telescope/telescope-ui-select.nvim',
    },
    cmd = { 'Telescope' },
    keys = {
      { '<Leader>..', '<cmd>Telescope find_files<cr>', desc = 'Open file' },
      { '<Leader>.b', '<cmd>Telescope buffers<cr>', desc = 'Jump to buffer' },
      { '<Leader>.g', '<cmd>Telescope live_grep<cr>', desc = 'Live grep' },
      { '<Leader>.h', '<cmd>Telescope help_tags<cr>', desc = 'Open help' },
      -- TODO: Consider moving next two keymaps to lspconfig.lua and changing prefix
      { '<Leader>.s', '<cmd>Telescope lsp_document_symbols<cr>', desc = 'Jump to symbol' },
      { '<Leader>lr', '<cmd>Telescope lsp_references<cr>', desc = 'References' },
      { '<Leader>gl', '<cmd>Telescope git_bcommits<cr>', desc = 'Log (buffer only)' },
      { '<Leader>gL', '<cmd>Telescope git_commits<cr>', desc = 'Log (everything)' },
      { '<Leader>gs', '<cmd>Telescope git_status<cr>', desc = 'Status' },
      { '<space>', '<cmd>Telescope buffers show_all_buffers=true<cr>', desc = 'Jump to buffer' },
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
            ['<Esc>'] = function(...) return require('telescope.actions').close(...) end,
            ['<C-t>'] = function(...) return require('trouble.providers.telescope').open_with_trouble(...) end,
            -- TODO: Consider adding keymaps similar to the following
            -- ['<a-i>'] = function() Util.telescope('find_files', { no_ignore = true })() end,
            -- ['<a-h>'] = function() Util.telescope('find_files', { hidden = true })() end,
          },
          n = {
            ['<C-t>'] = function(...) return require('trouble.providers.telescope').open_with_trouble(...) end,
          },
        },
      },
      pickers = {
        find_files = {
          find_command = { 'rg', '--ignore', '--hidden', '--files' },
        },
      },
      extensions = {
        ['fzf'] = {},
        -- TODO: Double-check whether this is working
        ['ui-select'] = { function(...) return require('telescope.themes').get_dropdown(...) end },
      },
    },
    config = function(_, opts)
      require('telescope').setup(opts)
      require('telescope').load_extension('fzf')
      require('telescope').load_extension('ui-select')
    end,
  },
}

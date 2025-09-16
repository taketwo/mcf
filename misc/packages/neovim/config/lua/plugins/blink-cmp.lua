return {
  {
    'saghen/blink.cmp',
    version = '*', -- Using a release tag to download pre-built binaries
    dependencies = {
      'rafamadriz/friendly-snippets',
      'xzbdmw/colorful-menu.nvim',
      -- Community completion sources
      'bydlw98/blink-cmp-env',
      'disrupted/blink-cmp-conventional-commits',
      'folke/lazydev.nvim',
    },
    event = { 'InsertEnter', 'CmdlineEnter' },

    ---@module 'blink.cmp'
    ---@type blink.cmp.Config
    opts = {
      snippets = {
        expand = function(snippet, _) return LazyVim.cmp.expand(snippet) end,
      },
      keymap = {
        preset = 'none',
        ['<C-space>'] = { 'show', 'show_documentation', 'hide_documentation' },
        ['<C-c>'] = { 'select_prev', 'fallback' },
        ['<C-d>'] = { 'scroll_documentation_up' },
        ['<C-f>'] = { 'scroll_documentation_down' },
        ['<C-t>'] = { 'select_next', 'fallback' },
        ['<C-h>'] = { 'cancel', 'fallback' },
        ['<C-n>'] = { 'select_and_accept', 'fallback' },
        ['<Tab>'] = { 'snippet_forward', 'fallback' },
        ['<S-Tab>'] = { 'snippet_backward', 'fallback' },
      },

      cmdline = {
        enabled = true,
        keymap = {
          preset = 'none',
          ['<C-c>'] = { 'select_prev', 'fallback' },
          ['<C-h>'] = { 'cancel', 'fallback' },
          ['<C-n>'] = { 'select_and_accept', 'fallback' },
          ['<C-t>'] = { 'select_next', 'fallback' },
          ['<S-Tab>'] = { 'select_prev', 'fallback' },
          ['<Tab>'] = { 'select_next', 'fallback' },
        },
        completion = {
          list = { selection = { preselect = false } },
          menu = {
            auto_show = function(_) return vim.fn.getcmdtype() == ':' end,
          },
        },
      },

      completion = {
        list = {
          selection = {
            preselect = false,
            auto_insert = false, -- Completion is inserted only after we accepted it
          },
        },
        documentation = {
          auto_show = true,
          auto_show_delay_ms = 1000,
        },
        ghost_text = { enabled = false },
        menu = {
          draw = {
            columns = {
              { 'kind_icon' },
              { 'label' },
              { 'source_name' },
            },
            components = {
              label = {
                width = { fill = true, min = 60, max = 60 },
                text = function(ctx) return require('colorful-menu').blink_components_text(ctx) end,
                highlight = function(ctx) return require('colorful-menu').blink_components_highlight(ctx) end,
              },
              kind_icon = {
                width = { min = 3 },
                text = function(ctx)
                  local kind_icon, _, _ = require('mini.icons').get('lsp', ctx.kind)
                  return kind_icon
                end,
              },
            },
          },
        },
      },
      signature = {
        enabled = true, -- Need to enable explicitly because this is experimental
        window = { border = 'single' },
      },
      sources = {
        default = {
          'lsp',
          'path',
          'snippets',
          'buffer',
          -- TODO: Migrate 'rosmsg' source to blink and enable it
        },
        per_filetype = {
          gitcommit = { inherit_defaults = true, 'cc' },
          lua = { inherit_defaults = true, 'lazydev' },
          sh = { inherit_defaults = true, 'env' },
        },
        providers = {
          cc = {
            name = 'Conventional commits',
            module = 'blink-cmp-conventional-commits',
          },
          env = {
            name = 'Environment variables',
            module = 'blink-cmp-env',
          },
          lazydev = {
            name = 'LazyDev',
            module = 'lazydev.integrations.blink',
            score_offset = 100,
          },
        },
      },
    },
    opts_extend = { 'sources.default' },
  },
}

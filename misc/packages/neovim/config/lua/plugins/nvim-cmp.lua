return {
  {
    'hrsh7th/nvim-cmp',
    event = 'InsertEnter',
    dependencies = {
      'hrsh7th/cmp-buffer',
      'hrsh7th/cmp-nvim-lsp',
      'hrsh7th/cmp-omni',
      'hrsh7th/cmp-path',
      'onsails/lspkind-nvim',
      'quangnguyen30192/cmp-nvim-ultisnips',
    },
    opts = function()
      local cmp = require('cmp')
      return {
        snippet = {
          expand = function(args) vim.fn['UltiSnips#Anon'](args.body) end,
        },
        mapping = {
          ['<C-d>'] = cmp.mapping.scroll_docs(-4),
          ['<C-f>'] = cmp.mapping.scroll_docs(4),
          ['<C-Space>'] = cmp.mapping.complete(),
          ['<C-h>'] = cmp.mapping.close(),
          ['<C-n>'] = cmp.mapping.confirm({ select = true }),
          ['<C-t>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
          ['<C-c>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
        },
        sources = cmp.config.sources({
          { name = 'nvim_lsp' },
          { name = 'ultisnips' },
          -- { name = 'omni' },  disable temporarily because of slow-downs when editing gitcommit
          { name = 'path' },
          { name = 'rosmsg' },
          { name = 'buffer', keyword_length = 4 },
        }),
        -- completion = {
        -- completeopt = 'menu,menuone,noinsert',
        -- },
        formatting = {
          format = function(entry, vim_item)
            -- At the moment 'cmp' does not support fixed-width windows, so we truncate the items
            -- explicitly to a maximum of 50 characters.
            vim_item.abbr = string.sub(vim_item.abbr, 1, 50)
            return require('lspkind').cmp_format({
              mode = 'symbol_text',
              symbol_map = {
                Class = '',
                Constant = '',
                Constructor = '',
                Enum = '',
                EnumMember = '',
                File = '',
                Folder = '',
                Function = '',
                Interface = '禍',
                Keyword = '',
                Method = '',
                Module = '',
                Operator = '洛',
                Property = '綠',
                Reference = '',
                Struct = '',
                Text = '',
                Snippet = '',
                Value = '',
                Variable = '',
              },
              menu = {
                buffer = '[Buffer]',
                path = '[Path]',
                omni = '[Omni]',
                nvim_lsp = '[LSP]',
                ultisnips = '[UltiSnips]',
              },
            })(entry, vim_item)
          end,
        },
        experimental = {
          native_menu = false,
          ghost_text = false,
        },
        window = {
          documentation = cmp.config.window.bordered(),
          completion = cmp.config.window.bordered(),
        },
      }
    end,
  },
}
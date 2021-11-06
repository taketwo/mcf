local cmp = require'cmp'

cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["UltiSnips#Anon"](args.body)
    end,
  },
  mapping = {
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<C-b>'] = cmp.mapping.confirm({ select = true }),
    ['<C-n>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
    ['<C-h>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
    ['<C-t>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
    ['<C-c>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
  },
  sources = {
    { name = 'nvim_lsp' },
    { name = 'ultisnips' },
    { name = 'omni' },
    { name = 'path' },
    { name = 'rosmsg' },
    { name = 'buffer', keyword_length = 4 },
  },
  formatting = {
    format = require'lspkind'.cmp_format({
      with_text = true,
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
        UltiSnips = '',
        Snippet = '',
        Value = '',
        Variable = '',
      },
      menu = ({
        buffer = "[Buffer]",
        path = "[Path]",
        omni = "[Omni]",
        nvim_lsp = "[LSP]",
        ultisnips = "[UltiSnips]",
      })
    }),
  },
  experimental = {
    native_menu = false,
    ghost_text = false,
  },
})


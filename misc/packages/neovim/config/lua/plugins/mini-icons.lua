return {
  {
    'echasnovski/mini.icons',
    lazy = true,
    opts = {
      lsp = {
        ['class'] = { glyph = '󰆧' },
        ['color'] = { glyph = '󰏘' },
        ['constant'] = { glyph = '󰏿' },
        ['constructor'] = { glyph = '' },
        ['enum'] = { glyph = '' },
        ['enum_member'] = { glyph = '' },
        ['event'] = { glyph = '' },
        ['field'] = { glyph = '󰜢' },
        ['file'] = { glyph = '󰈙' },
        ['folder'] = { glyph = '' },
        ['function'] = { glyph = '󰊕' },
        ['interface'] = { glyph = '' },
        ['keyword'] = { glyph = '󰌋' },
        ['method'] = { glyph = '󰊕' },
        ['module'] = { glyph = '󰕳' },
        ['operator'] = { glyph = '󰆕' },
        ['property'] = { glyph = '󰜢' },
        ['reference'] = { glyph = '󰈇' },
        ['snippet'] = { glyph = '' },
        ['struct'] = { glyph = '󰙅' },
        ['text'] = { glyph = '' },
        ['type_parameter'] = { glyph = '󰊄' },
        ['unit'] = { glyph = '󰑭' },
        ['value'] = { glyph = '󰎠' },
        ['variable'] = { glyph = '󰀫' },
      },
    },
    init = function()
      package.preload['nvim-web-devicons'] = function()
        require('mini.icons').mock_nvim_web_devicons()
        return package.loaded['nvim-web-devicons']
      end
    end,
  },
}

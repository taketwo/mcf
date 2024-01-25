return {
  {
    'navarasu/onedark.nvim',
    lazy = false, -- main colorscheme should be loaded during startup
    priority = 1000, -- main colorscheme should be loaded before other plugins
    opts = {
      style = 'warmer',
      transparent = true,
      highlights = {
        Search = { fg = 'none', bg = 'none', fmt = 'bold,underline' },
        CurSearch = { fg = 'none', bg = 'none', fmt = 'bold,underline' },
        IncSearch = { fg = 'none', bg = 'none', fmt = 'bold,underline,standout' },
        CursorLineNr = { fmt = 'bold' },
        YankedRegion = { fg = '$red' },
        FloatBorder = { bg = 'none' },
        IlluminatedWordText = { fmt = 'bold' },
        IlluminatedWordRead = { fmt = 'bold' },
        IlluminatedWordWrite = { fmt = 'bold' },
        MatchParen = { fg = '$red', bg = 'none', fmt = 'bold' }, -- Matching parenthesis
        MatchWord = { fg = 'none', bg = 'none', fmt = 'nocombine' }, -- Matching word/tag (disable highlighting)
        NeoTreeIndentMarker = { fg = '$grey' },
        NeoTreeExpander = { fg = '$grey' },
        NeoTreeModified = { fg = '$orange' },
      },
    },
    config = function(_, opts)
      local onedark = require('onedark')
      onedark.setup(opts)
      onedark.load()
    end,
  },
}

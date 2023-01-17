local onedark = require('onedark')

onedark.setup({
  style = 'warmer',
  transparent = true,
  highlights = {
    Search = { fg = 'none', bg = 'none', fmt = 'bold,underline' },
    IncSearch = { fg = 'none', bg = 'none', fmt = 'bold,underline,standout' },
    CursorLineNr = { fmt = 'bold' },
    YankedRegion = { fg = '$red' },
    FloatBorder = { bg = 'none' },
    IlluminatedWordText = { fmt = 'bold' },
    IlluminatedWordRead = { fmt = 'bold' },
    IlluminatedWordWrite = { fmt = 'bold' },
  },
})

onedark.load()

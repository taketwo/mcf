local onedark = require('onedark')

onedark.setup{
  style = 'warmer',
  transparent = true,
  highlights = {
    Search = { fg = 'none', bg = 'none', fmt = 'bold,underline' },
    IncSearch = { fg = 'none', bg = 'none', fmt = 'bold,underline,standout' },
    CursorLineNr = { fmt = 'bold' },
    YankedRegion = { fg = '$red' },
  }
}

onedark.load()

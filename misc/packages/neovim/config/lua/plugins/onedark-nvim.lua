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
      },
    },
    config = function(_, opts)
      local onedark = require('onedark')
      onedark.setup(opts)
      onedark.load()
    end,
  },
}

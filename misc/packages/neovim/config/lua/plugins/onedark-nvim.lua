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
        -- The highlight below changes the background of inactive Neovim windows in the same way as it does with inactive
        -- Tmux panes. On one hand, this is useful to understand which window is active. On the other hand, it destroys the
        -- visual consistency of floating windows (such as Telescope). Therefore, it is commented out.
        -- NormalNC = { fg = 'none', bg = '$bg1' },
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

return {
  {
    'navarasu/onedark.nvim',
    lazy = false, -- main colorscheme should be loaded during startup
    priority = 1000, -- main colorscheme should be loaded before other plugins
    opts = {
      style = 'warmer',
      transparent = true,
      highlights = {
        -- Background transparency setup
        -- We want Neovim windows to have the same background as Tmux panes that they are in. (This allows to easily
        -- differentiate between focused and unfocused panes.) The `transparent` option handles this for the most part,
        -- but the floating windows (such as Telescope or Snacks Picker) require additional configuration.
        NormalFloat = { bg = 'none' },
        FloatBorder = { bg = 'none' },
        -- The highlight below changes the background of inactive Neovim windows in the same way as it does with inactive
        -- Tmux panes. On one hand, this is useful to understand which window is active. On the other hand, it destroys the
        -- visual consistency of floating windows (such as Telescope). Therefore, it is commented out.
        -- NormalNC = { fg = 'none', bg = '$bg1' },
        Search = { fg = 'none', bg = 'none', fmt = 'bold,underline' },
        CurSearch = { fg = 'none', bg = 'none', fmt = 'bold,underline' },
        IncSearch = { fg = 'none', bg = 'none', fmt = 'bold,underline,standout' },
        CursorLineNr = { fmt = 'bold' },
        YankedRegion = { fg = '$red' },
        IlluminatedWordText = { fmt = 'bold', bg = 'none' },
        IlluminatedWordRead = { fmt = 'bold', bg = 'none' },
        IlluminatedWordWrite = { fmt = 'bold', bg = 'none' },
        MatchParen = { fg = '$red', bg = 'none', fmt = 'bold' }, -- Matching parenthesis
        MatchWord = { fg = 'none', bg = 'none', fmt = 'nocombine' }, -- Matching word/tag (disable highlighting)
        NeoTreeIndentMarker = { fg = '$grey' },
        NeoTreeExpander = { fg = '$grey' },
        NeoTreeModified = { fg = '$orange' },
        LeapBackdrop = { fg = '$grey' },
        LeapMatch = { fg = 'none', bg = '$red', fmt = 'bold' },
        LeapLabel = { fg = 'none', bg = '$blue', fmt = 'bold' },
        BufferCurrent = { fg = '$fg', bg = '$bg0', fmt = 'bold' },
        BufferCurrentMod = { fg = '$orange', bg = '$bg0', fmt = 'bold,italic' },
        DiffviewDiffDeleteDim = { fg = '$bg2' },
        Folded = { bg = '$bg1' },
        SnacksIndent = { fg = '$bg1', fmt = 'nocombine' },
      },
    },
    config = function(_, opts)
      local onedark = require('onedark')
      onedark.setup(opts)
      onedark.load()
    end,
  },
}

return {
  {
    'scrooloose/nerdcommenter',
    init = function()
      vim.g.NERDCompactSexyComs = 1 -- Use compact style of "sexy" comments
      vim.g.NERDSpaceDelims = 1 -- Add spaces after the left delimiter and before the right delimiter
    end,
    -- TODO: Use JoosepAlviste/nvim-ts-context-commentstring to decide commentstring based on context
  },
}

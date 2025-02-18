return {
  {
    'MagicDuck/grug-far.nvim',
    cmd = 'GrugFar',
    ---@type GrugFarOptions
    ---@diagnostic disable: missing-fields
    opts = {
      headerMaxWidth = 80,
      keymaps = {
        close = { n = 'q' },
      },
    },
  },
}

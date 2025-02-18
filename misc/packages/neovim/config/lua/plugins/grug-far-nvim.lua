return {
  {
    'MagicDuck/grug-far.nvim',
    cmd = 'GrugFar',
    ---@type GrugFarOptions
    ---@diagnostic disable: missing-fields
    opts = {
      headerMaxWidth = 80,
      openTargetWindow = {
        preferredLocation = 'right',
      },
      keymaps = {
        openNextLocation = { n = '>' },
        openPrevLocation = { n = '<' },
        openLocation = { n = 'o' },
        close = { n = 'q' },
      },
    },
  },
}

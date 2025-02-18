return {
  {
    'MagicDuck/grug-far.nvim',
    cmd = 'GrugFar',
    keys = {
      {
        '<C-f>',
        function() require('grug-far').open({ transient = true }) end,
        desc = 'Find/replace',
        mode = { 'n', 'x' },
      },
    },
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

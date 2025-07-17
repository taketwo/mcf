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
        nextInput = { n = '<Tab', i = '<Tab>' },
        prevInput = { n = '<S-Tab', i = '<S-Tab>' },
        close = { n = 'q' },
      },
    },
    config = function(_, opts)
      require('grug-far').setup(opts)
      -- Autocommand to create custom keymaps in grug-far buffer
      vim.api.nvim_create_autocmd('FileType', {
        group = vim.api.nvim_create_augroup('grug-far-keymap', { clear = true }),
        pattern = { 'grug-far' },
        callback = function()
          -- Leave insert mode and jump to first result with Enter
          vim.keymap.set({ 'i' }, '<Enter>', '<Esc>>', { buffer = true, remap = true })
          -- Open location and close Grug-far window with O
          vim.keymap.set(
            { 'n' },
            'O',
            '<LocalLeader>oq',
            { buffer = true, remap = true, desc = 'Open location and close Grug-far' }
          )
        end,
      })
    end,
  },
}

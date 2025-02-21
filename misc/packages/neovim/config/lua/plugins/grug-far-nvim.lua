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
          local function jump_field(direction)
            local MIN_FIELD_LINE = 2
            local MAX_FIELD_LINE = 6
            local current_line = vim.api.nvim_win_get_cursor(0)[1]
            local next_line = current_line + direction
            if next_line > MAX_FIELD_LINE then
              next_line = MIN_FIELD_LINE
            elseif next_line < MIN_FIELD_LINE then
              next_line = MAX_FIELD_LINE
            end
            vim.api.nvim_win_set_cursor(vim.fn.bufwinid(0), { next_line, 0 })
          end
          -- Jump between input fields with Tab and Shift-Tab
          vim.keymap.set({ 'n', 'i' }, '<Tab>', function() jump_field(1) end, { buffer = true })
          vim.keymap.set({ 'n', 'i' }, '<S-Tab>', function() jump_field(-1) end, { buffer = true })
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

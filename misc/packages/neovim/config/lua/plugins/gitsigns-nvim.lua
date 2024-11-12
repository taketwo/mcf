return {
  {
    'lewis6991/gitsigns.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        require('which-key').add({
          buffer = bufnr,
          {
            { '<Leader>g', group = 'Git' },
            { '<Leader>gb', gs.blame, desc = 'Blame buffer' },
            { '<Leader>gd', gs.toggle_deleted, desc = 'Toggle deleted lines' },
            { '<Leader>gh', group = 'Hunks' },
            {
              '<Leader>gh<space>',
              function() require('which-key').show({ keys = '<Leader>gh', loop = true }) end,
              desc = 'Hydra mode',
            },
            { '<Leader>gh]', gs.nav_hunk('next'), desc = 'Next' },
            { '<Leader>gh[', gs.nav_hunk('prev'), desc = 'Previous' },
            { '<Leader>ghd', gs.diffthis, desc = 'Diff' },
            { '<Leader>ghp', gs.preview_hunk_inline, desc = 'Preview inline' },
            { '<Leader>ghr', gs.reset_hunk, desc = 'Reset' },
            {
              '<Leader>ghs',
              function()
                gs.stage_hunk()
                gs.refresh()
              end,
              desc = 'Stage',
            },
            { '<Leader>ghu', gs.undo_stage_hunk, desc = 'Unstage' },
            { '<Leader>gh]', function() gs.nav_hunk('next') end, desc = 'Next' },
            { '<Leader>gh[', function() gs.nav_hunk('prev') end, desc = 'Previous' },
            { '<Leader>gs', gs.stage_buffer, desc = 'Stage buffer' },
            { '<Leader>ub', gs.toggle_current_line_blame, desc = 'Toggle current line blame' },
            {
              ']h',
              function()
                if vim.wo.diff then
                  vim.cmd.normal({ ']c', bang = true })
                else
                  gs.nav_hunk('next')
                end
              end,
              desc = 'Next git hunk or diff',
            },
            {
              '[h',
              function()
                if vim.wo.diff then
                  vim.cmd.normal({ '[c', bang = true })
                else
                  gs.nav_hunk('prev')
                end
              end,
              desc = 'Previous git hunk or diff',
            },
            -- Text object
            { 'ih', ':<C-U>Gitsigns select_hunk<CR>', desc = 'Git hunk', mode = { 'o', 'x' } },
          },
        })
      end,
    },
  },
}

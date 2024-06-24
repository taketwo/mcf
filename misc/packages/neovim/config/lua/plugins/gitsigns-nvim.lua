return {
  {
    'lewis6991/gitsigns.nvim',
    event = { 'BufReadPre', 'BufNewFile' },
    opts = {
      on_attach = function(bufnr)
        local gs = package.loaded.gitsigns
        require('which-key').register({
          ['<Leader>'] = {
            g = {
              name = 'Git',
              d = { gs.toggle_deleted, 'Toggle deleted lines' },
              h = {
                name = 'Hunks',
                d = { gs.diffthis, 'Diff' },
                p = { gs.preview_hunk_inline, 'Preview inline' },
                r = { gs.reset_hunk, 'Reset' },
                s = { gs.stage_hunk, 'Stage' },
                u = { gs.undo_stage_hunk, 'Unstage' },
              },
              -- TODO: The one below is an alternative to git-messenger, consider removing one of them
              M = { function() gs.blame_line({ full = true }) end, 'View commit message (gitsigns)' },
              s = { gs.stage_buffer, 'Stage buffer' },
            },
            u = {
              b = { gs.toggle_current_line_blame, 'Toggle current line blame' },
            },
          },
          [']h'] = {
            function()
              if vim.wo.diff then
                vim.cmd.normal({ ']c', bang = true })
              else
                gs.nav_hunk('next')
              end
            end,
            'Next git hunk or diff',
          },
          ['[h'] = {
            function()
              if vim.wo.diff then
                vim.cmd.normal({ '[c', bang = true })
              else
                gs.nav_hunk('prev')
              end
            end,
            'Previous git hunk or diff',
          },
          -- Text object
          ih = { ':<C-U>Gitsigns select_hunk<CR>', 'Git hunk', mode = { 'o', 'x' } },
        }, { buffer = bufnr })
      end,
    },
  },
}

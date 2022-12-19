require('gitsigns').setup{
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    -- TODO: Make the mappings local to the buffer
    require('which-key').register{
      ['<Leader>'] = {
        g = {
          name = 'Git',
          b = { '<cmd>Git blame<cr>', 'Blame' },
          h = {
            name = '+hunks',
            d = { gs.diffthis, 'Diff' },
            p = { gs.preview_hunk, 'Preview' },
            r = { '<cmd>Gitsigns reset_hunk<cr>', 'Reset' },
            s = { '<cmd>Gitsigns stage_hunk<cr>', 'Stage' },
            u = { gs.undo_stage_hunk, 'Unstage' },
          },
          -- TODO: The one below is an alternative to git-messenger, consider removing one of them
          B = { function() gs.blame_line{full=true} end, 'Blame' },
          tb = { gs.toggle_current_line_blame, 'Toggle current line blame' },
          td = { gs.toggle_deleted, 'Toggle deleted lines' },
        },
      },
      [']h'] = { function()
          if vim.wo.diff then return ']h' end
          vim.schedule(function() gs.next_hunk() end)
          return '<Ignore>'
        end, 'Next git hunk', expr=true },
      ['[h'] = { function()
          if vim.wo.diff then return '[h' end
          vim.schedule(function() gs.prev_hunk() end)
          return '<Ignore>'
        end, 'Previous git hunk', expr=true },
    }

    -- Text object
    require('which-key').register{
      ih = { ':<C-U>Gitsigns select_hunk<CR>', 'Select hunk', mode = { 'o', 'x' }  }
    }
  end
}

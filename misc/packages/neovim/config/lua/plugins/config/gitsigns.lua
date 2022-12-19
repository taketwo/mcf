require('gitsigns').setup{
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    -- TODO: Move some of the keymaps out of 'h' subsection
    require('which-key').register{
      ['<Leader>gh'] = {
        name = '+hunks',
        b = { function() gs.blame_line{full=true} end, 'Blame' },
        d = { gs.diffthis, 'Diff' },
        p = { gs.preview_hunk, 'Preview' },
        r = { '<cmd>Gitsigns reset_hunk<cr>', 'Reset' },
        s = { '<cmd>Gitsigns stage_hunk<cr>', 'Stage' },
        tb = { gs.toggle_current_line_blame, 'Toggle current line blame' },
        td = { gs.toggle_deleted, 'Toggle deleted lines' },
        u = { gs.undo_stage_hunk, 'Unstage' },
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

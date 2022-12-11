require("which-key").register{
  ['<Leader>gh'] = {
    name = '+hunks',
    p = { '<cmd>GitGutterPreviewHunk<cr>', 'Preview' },
    s = { '<cmd>GitGutterStageHunk<cr>', 'Stage' },
    u = { '<cmd>GitGutterUndoHunk<cr>', 'Undo' },
  },
  [']h'] = { '<cmd>GitGutterNextHunk<cr>', 'Next git hunk' },
  ['[h'] = { '<cmd>GitGutterPrevHunk<cr>', 'Previous git hunk' },
}

local keymap = vim.api.nvim_set_keymap
keymap('o', 'ih', '<Plug>(GitGutterTextObjectInnerPending)', { noremap=false, desc='Hunk' })
keymap('o', 'ah', '<Plug>(GitGutterTextObjectOuterPending)', { noremap=false, desc='Hunk and trailiing empty lines' })
keymap('x', 'ih', '<Plug>(GitGutterTextObjectInnerVisual)', { noremap=false, desc='Hunk' })
keymap('x', 'ah', '<Plug>(GitGutterTextObjectOuterVisual)', { noremap=false, desc='Hunk and trailiing empty lines' })

-- Disable default keymaps
vim.g.gitgutter_map_keys = 0
-- GitGutter signs should have low priority to avoid clobbering important signs (e.g. from LSP)
vim.g.gitgutter_sign_priority = 1
-- This seems to not have any effect, keeping just in case
vim.g.gitgutter_sign_allow_clobber = 0
-- Sign characters
vim.g.gitgutter_sign_added = '▌'
vim.g.gitgutter_sign_modified = '▌'
vim.g.gitgutter_sign_removed = '▁'
vim.g.gitgutter_sign_removed_first_line = '▔'
vim.g.gitgutter_sign_modified_removed = '▁'

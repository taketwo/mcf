require('which-key').register({
  [']]'] = { require('illuminate').goto_next_reference, 'Go to next reference' },
  ['[['] = { require('illuminate').goto_prev_reference, 'Go to previous reference' },
  -- ['ir'] = { require('illuminate').textobj_select, 'reference', mode = { 'o', 'x' } },
})

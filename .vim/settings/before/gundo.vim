let g:gundo_map_move_older = 't'
let g:gundo_map_move_newer = 'c'
" Automatically close the Gundo windows when reverting
let g:gundo_close_on_revert = 1

if has('python3')
  let g:gundo_prefer_python3 = 1
endif

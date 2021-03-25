" Navigate between hunks
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)

" Hunk text object
omap ih <Plug>(GitGutterTextObjectInnerPending)
omap ah <Plug>(GitGutterTextObjectOuterPending)
xmap ih <Plug>(GitGutterTextObjectInnerVisual)
xmap ah <Plug>(GitGutterTextObjectOuterVisual)

highlight GitGutterAdd ctermfg=70
highlight GitGutterChange ctermfg=69
highlight GitGutterDelete ctermfg=124

" GitGutter signs should have low priority to avoid clobbering important signs (e.g. from LSP)
let g:gitgutter_sign_priority = 1
" This seems to have no effect, keeping just in case
let g:gitgutter_sign_allow_clobber = 0

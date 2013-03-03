" SuperTab option for context aware completion
let g:SuperTabDefaultCompletionType = "context"
" We do not use ContextDiscover because when it fails
" there is no way to chain keyword completion
let g:SuperTabCompletionContexts = ['s:ContextText']
" If context detection did not yield a completion type, then
" fall back to omnicompletion
let g:SuperTabContextDefaultCompletionType = "<c-x><c-o>"
" Trigger with Ctrl + Space
let g:SuperTabMappingForward = '<C-@>'
let g:SuperTabMappingBackward = '<S-C-@>'
" Pre-select the longest match
let g:SuperTabLongestHighlight = 1
" Auto close the preview window
let g:SuperTabClosePreviewOnPopupClose = 1
" If omnifunc is defined, chain keyword completion to it
autocmd FileType *
  \ if &omnifunc != '' |
  \   call SuperTabChain(&omnifunc, "<c-p>") |
  \ endif

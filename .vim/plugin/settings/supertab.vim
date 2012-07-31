" SuperTab option for context aware completion
"let g:SuperTabDefaultCompletionType = "context"
" Pre-select the longest match
let g:SuperTabLongestHighlight = 1
let g:SuperTabCompletionContexts = ['s:ContextDiscover', 's:ContextText']
let g:SuperTabContextTextOmniPrecedence = ['&omnifunc', '&completefunc']
let g:SuperTabContextDiscoverDiscovery =
    \ ["&omnifunc:<c-x><c-o>","&completefunc:<c-x><c-u>"]
let g:SuperTabMappingForward = '<tab>'
let g:SuperTabMappingBackward = '<s-tab>'

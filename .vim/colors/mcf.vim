runtime colors/solarized8.vim

highlight LineNr guifg=#93a1a1 guibg=#eee8d5
highlight SignColumn guifg=#93a1a1 guibg=#eee8d5
highlight Search gui=italic,bold guifg=#dc322f
highlight IncSearch gui=italic,bold,standout guifg=#dc322f
" TODO: Review and maybe enable these
" highlight Whitespace ctermfg=7 cterm=nocombine
" highlight SpellBad ctermfg=red                " Adjust the color of wrongly spelled words

" FIXME: Does not work
" highlight CursorLineNr guifg=#93a1a1 guibg=#eee8d5 gui=bold   " Show current line numbers in bold

" Diagnostics
" TODO: Review and maybe update colors
highlight! link DiagnosticSignError ALEErrorSign
highlight! link DiagnosticSignWarn ALEWarningSign
highlight! link DiagnosticSignInfo ALEWarningInfo
" highlight! link DiagnosticSignHint ALEWarningInfo

highlight LspReferenceRead gui=bold
highlight LspReferenceText gui=bold
highlight LspReferenceWrite gui=bold

" Matchup
" TODO: Review and maybe enable this
" highlight MatchParen cterm=bold
" highlight MatchWord cterm=bold

" GitGutter
highlight GitGutterAdd guifg=#859900 guibg=#eee8d5
highlight GitGutterChange guifg=#268bd2 guibg=#eee8d5
highlight GitGutterDelete guifg=#dc322f guibg=#eee8d5

" IndentBlankline
highlight IndentBlanklineChar guifg=#eee8d5 gui=nocombine
highlight IndentBlanklineContextChar guifg=#ded8c5 gui=nocombine

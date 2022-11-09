runtime colors/solarized.vim

highlight SpellBad ctermfg=red                " Adjust the color of wrongly spelled words
highlight Normal ctermbg=NONE                 " Remove explicit background color (inherit from Tmux)
highlight CursorLineNr cterm=bold ctermbg=7   " Show current line numbers in bold
highlight LineNr ctermfg=14 ctermbg=7
highlight Search cterm=italic,bold ctermfg=red
highlight IncSearch cterm=italic,bold,standout ctermfg=red
highlight Whitespace ctermfg=7 cterm=nocombine

highlight DiagnosticError ctermfg=160
highlight DiagnosticWarn ctermfg=166

highlight DiagnosticSignError ctermfg=160 ctermbg=7
highlight DiagnosticSignWarn ctermfg=166 ctermbg=7

highlight LspReferenceRead cterm=bold
highlight LspReferenceText cterm=bold
highlight LspReferenceWrite cterm=bold

" GitGutter
highlight GitGutterAdd ctermfg=70 ctermbg=7
highlight GitGutterChange ctermfg=69 ctermbg=7
highlight GitGutterDelete ctermfg=124 ctermbg=7

" Matchup
highlight MatchParen cterm=bold
highlight MatchWord cterm=bold

" IndentBlankline
highlight IndentBlanklineContextChar ctermfg=14 cterm=nocombine

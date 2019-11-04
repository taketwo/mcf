" Get documentation for the word under cursor, when idle.
function! <SID>AutoGetDocToggle()
    if exists('#auto_getdoc')
        au! auto_getdoc
        augroup! auto_getdoc
            set updatetime=4000
            echo 'Get documentation for current word: off'
            return 0
        else
            augroup auto_getdoc
                au!
                au CursorHold * silent! :YcmCompleter GetDocImprecise
            augroup end
            set updatetime=500
            echo 'Get documentation for current word: ON'
            return 1
    endif
endfunction
command! AutoGetDocToggle call <SID>AutoGetDocToggle()

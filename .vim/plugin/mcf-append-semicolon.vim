" If there isn't one, append a semicolon to the end of the current line
" Stolen from skwp/dotfiles
function! AppendSemicolon()
    if getline('.') !~ ';$'
        let original_cursor_position = getpos('.')
        exec("s/$/;/")
        call setpos('.', original_cursor_position)
    endif
endfunction

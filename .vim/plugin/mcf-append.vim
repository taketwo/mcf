" Append a string to the end of the current line without moving the cursor.
" (Has no effect if the line already ends with the same string.)
" Stolen from skwp/dotfiles
function! AppendString(string)
    if getline('.') !~ a:string.'$'
        let original_cursor_position = getpos('.')
        exec("s/$/".a:string."/")
        call setpos('.', original_cursor_position)
    endif
endfunction

" Handy for C++
function! AppendSemicolon()
    call AppendString(";")
endfunction

" Handy for Python
function! AppendColon()
    call AppendString(":")
endfunction

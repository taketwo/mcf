" Append a string to the end of the current line without moving the cursor.
" (Has no effect if the line already ends with the same string.)
" Inspired by skwp/dotfiles and http://superuser.com/a/691230/210066
function! AppendString(string)
    let line = getline('.')
    if line !~ a:string . '$'
        call setline('.', line . a:string)
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

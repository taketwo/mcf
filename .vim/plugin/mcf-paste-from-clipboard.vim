" Enable paste mode, paste the contents of the system clipboard
" Should be called from insert mode
function! PasteFromClipboard() abort
    let s:paste = &paste
    set paste
    !xdotool key ctrl+shift+v
endfunction

augroup PasteAutoDisable
    autocmd!
    autocmd InsertLeave *
        \ if exists('s:paste') |
        \     let &paste = s:paste |
        \     unlet s:paste |
        \ endif
augroup END

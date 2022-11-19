if has('nvim')
    " With Neovim it's very simple: just paste from + register
    function! PasteFromClipboard() abort
        normal "+P
    endfunction
else
    " With Vim it's complicated: enable paste mode, paste the contents of the system clipboard
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
endif

for v in ['5.0', '4.0', '3.8', '3.6']
    let e = 'clang-format-' . v
    if executable(e)
        let g:clang_format#command = e
        break
    endif
endfor

autocmd FileType c,cpp,objc nnoremap <buffer><LocalLeader><Space> :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><LocalLeader><Space> :ClangFormat<CR>

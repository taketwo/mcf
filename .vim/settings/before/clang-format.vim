let g:clang_format#command = 'clang-format-3.5'
autocmd FileType c,cpp,objc nnoremap <buffer><LocalLeader><Space> :<C-u>ClangFormat<CR>
autocmd FileType c,cpp,objc vnoremap <buffer><LocalLeader><Space> :ClangFormat<CR>

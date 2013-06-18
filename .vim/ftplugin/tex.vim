setlocal textwidth=79
setlocal colorcolumn=80
setlocal foldmethod=marker

" Set default completion type to keywords, there is no
" omnicompletion for tex anyways.
call SuperTabSetDefaultCompletionType("<c-p>")

nnoremap <buffer> <Space><Space> gq}

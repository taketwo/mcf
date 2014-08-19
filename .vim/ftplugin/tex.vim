setlocal textwidth=79
setlocal colorcolumn=80
setlocal foldmethod=marker

" Set default completion type to keywords, there is no
" omnicompletion for tex anyways.
if exists("SuperTabSetDefaultCompletionType")
    call SuperTabSetDefaultCompletionType("<c-p>")
endif

nnoremap <buffer> <Space><Space> gq}

" Set default completion type to keywords, there is no
" omnicompletion for haskell anyways.
if exists("SuperTabSetDefaultCompletionType")
    call SuperTabSetDefaultCompletionType("<c-p>")
endif
setlocal fdm=marker

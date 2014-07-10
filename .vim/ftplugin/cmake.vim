" Set default completion type to keywords, there is no
" omnicompletion for cmake anyways
if exists("SuperTabSetDefaultCompletionType")
  call SuperTabSetDefaultCompletionType("<c-p>")
endif

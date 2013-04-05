" Disable autocompletion for < in C-family languages
au FileType c,cpp,cuda let b:delimitMate_matchpairs = "(:),[:],{:}"

" Disable autocompletion for < in Python
au FileType python let b:delimitMate_matchpairs = "(:),[:],{:}"

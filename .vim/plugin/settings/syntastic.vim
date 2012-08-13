" mark syntax errors with :signs
let g:syntastic_enable_signs=1
" automatically jump to the error when saving the file
let g:syntastic_auto_jump=0
" show the error list automatically
let g:syntastic_auto_loc_list=1
" don't care about warnings
let g:syntastic_quiet_warnings=0
" set default compiler config
let g:syntastic_cpp_config_file='.clang_complete'
" set fancy error symbol
let g:syntastic_error_symbol='✘'
:highlight SignColumn ctermbg=bg

" mark syntax errors with :signs
let g:syntastic_enable_signs = 1
" automatically jump to the error when saving the file
let g:syntastic_auto_jump = 0
" show the error list automatically
let g:syntastic_auto_loc_list = 1
" don't care about warnings
let g:syntastic_quiet_warnings = 0
" set default compiler config
let g:syntastic_cpp_config_file = '.clang_complete'
" set fancy symbols
let g:syntastic_error_symbol = '✘'
let g:syntastic_warning_symbol = '⚠'
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'active_filetypes': ['python'],
                           \ 'passive_filetypes': ['cpp'] }
let g:syntastic_cpp_check_header = 0
:highlight SignColumn ctermbg=bg

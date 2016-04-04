" mark syntax errors with :signs
let g:syntastic_enable_signs = 1
" automatically jump to the error when saving the file
let g:syntastic_auto_jump = 0
" show the error list automatically, in location list
let g:syntastic_auto_loc_list = 1
let g:syntastic_always_populate_loc_list = 1
" set fancy symbols
let g:syntastic_error_symbol = '✸'
let g:syntastic_warning_symbol = '✶'
let g:syntastic_style_error_symbol = '✤'
let g:syntastic_style_warning_symbol = '✢'
let g:syntastic_mode_map = { 'mode': 'active',
                           \ 'passive_filetypes': ['cpp', 'tex', 'sh', 'vim', 'rst', 'cuda'] }
let g:syntastic_cpp_check_header = 0
let g:syntastic_python_checkers = ['frosted']

" Disable default mappings
let g:ipy_perform_mappings = 0

nnoremap <C-S> :python run_this_file()<CR>

nnoremap <C-s> :python run_this_line()<CR>
inoremap <C-s> <C-O>:python run_this_line()<CR>

vnoremap <C-s> :python run_these_lines()<CR>

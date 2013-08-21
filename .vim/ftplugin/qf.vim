" Navigate between quickfix items with 'h' (left) and 'n' (right)
nnoremap <buffer> h k
nnoremap <buffer> n j

" Jump to the next item in the list with Tab
nnoremap <Tab> :cn<CR>

" Jump to the error under cursor
nnoremap <buffer> <CR> :cc<CR>

resize 10
wincmd J

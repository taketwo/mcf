" Navigate between quickfix items with 'h' (left) and 'n' (right)
nnoremap <silent> <buffer> h k
nnoremap <silent> <buffer> n j

" Close quickfix
nnoremap <silent> <buffer> q :close<CR>

" No need for relative numbers in quickfix
setlocal norelativenumber

nnoremap <buffer> o :cc<CR>
nnoremap <buffer> <Return> :cc<CR>
nnoremap <buffer> <Tab> :cn<CR>
nnoremap <buffer> <S-Tab> :cp<CR>

resize 12
wincmd J

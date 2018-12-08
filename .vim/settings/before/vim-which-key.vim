nnoremap <silent> <Leader> :<c-u>WhichKey ','<CR>
nnoremap <silent> <LocalLeader> :<c-u>WhichKey '\'<CR>

autocmd! FileType which_key
autocmd  FileType which_key set laststatus=0 noshowmode noruler
  \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

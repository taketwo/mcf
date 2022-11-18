let maplocalleader = '\'

setlocal textwidth=80
setlocal colorcolumn=80
setlocal conceallevel=2

" Turn the current line into header
nnoremap <LocalLeader>1 :t.<CR>Vr=k
nnoremap <LocalLeader>2 :t.<CR>Vr-k
nnoremap <LocalLeader>3 I### <Esc>
nnoremap <LocalLeader>4 I#### <Esc>

let maplocalleader = '\'

setlocal textwidth=80
setlocal colorcolumn=80

" Turn the current line into header
nnoremap <LocalLeader>1 :t.<CR>Vr=k
nnoremap <LocalLeader>2 :t.<CR>Vr-k

nnoremap <buffer> <LocalLeader><Space> gq}

let maplocalleader = '\'

setlocal makeprg=asciidoctor\ \%

setlocal textwidth=80
setlocal colorcolumn=80

" Turn the current line into header
nnoremap <LocalLeader>1 I= <Esc>
nnoremap <LocalLeader>2 I== <Esc>
nnoremap <LocalLeader>3 I=== <Esc>
nnoremap <LocalLeader>4 I==== <Esc>

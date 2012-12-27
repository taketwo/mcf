let maplocalleader = '\'

setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal textwidth=80
setlocal colorcolumn=80
setlocal nosmartindent
setlocal completeopt=menu,menuone,longest,preview
autocmd InsertLeave * if pumvisible() == 0 | pclose | endif

" Remove function call
" When on a function name, removes the name and the parens around its arguments
nnoremap <LocalLeader>f diwds()

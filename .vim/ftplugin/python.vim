let maplocalleader = '\'

setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal colorcolumn=88
setlocal nosmartindent
setlocal completeopt=menu,menuone,longest,preview
autocmd InsertLeave * if pumvisible() == 0 | pclose | endif

" Remove function call
" When on a function name, removes the name and the parens around its arguments
nnoremap <LocalLeader>f diwds(

" Wrap with function call
xmap W <Plug>VSurround

" Hint: to get ASCII value put the cursor over a letter and press 'ga'

" w - input function name
let b:surround_119 = "\1Function: \1(\r)"
" l - len
let b:surround_108 = "len(\r)"
" r - range
let b:surround_114 = "range(\r)"
" e - enumerate
let b:surround_101 = "enumerate(\r)"

" Append colon to the end of line
nnoremap <buffer> <silent> <LocalLeader>: :call AppendColon()<CR>
inoremap <buffer> <silent> <LocalLeader>: <C-o>:call AppendColon()<CR>

" YCM mappings
nnoremap <buffer> gd :YcmCompleter GoTo<CR>
nnoremap <buffer> gr :YcmCompleter GoToReferences<CR>
nnoremap <silent> <LocalLeader>d :YcmCompleter GetDoc<CR>

" ALE config
" Turn flake8 style errors into warnings
let b:ale_type_map = {'flake8': {'ES': 'WS'}}
" Flake8 configuration, as recommened by Black
" Ignore
"  * E501 - strict line length, we use bugbear's relaxed B950 instead
"  * W503 (line break before binary operator) - this one is not compliant with PEP8
let b:ale_python_flake8_options = '--ignore=E501,W503 --select=C,E,F,W,B,B950 --max-line-length=80'
" Fixers
let b:ale_fixers = [ 'black' ]


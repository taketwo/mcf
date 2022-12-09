let maplocalleader = '\'

setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4
setlocal colorcolumn=88
setlocal nosmartindent
autocmd InsertLeave * if pumvisible() == 0 | pclose | endif

" Insert PUDB breakpoint after current line
function! InsertBreakpoint()
    call feedkeys("opudb\<C-R>=UltiSnips#ExpandSnippet()\<CR>", "x")
endfunction

let b:keymaps = {
  \   "b" : ["InsertBreakpoint()", "insert-breakpoint"]     ,
  \   "f" : ["normal diwds("     , "remove-function-call"]  ,
  \   ":" : ["AppendColon()"     , "append-colon"]          ,
  \ }

if exists("which_key#register")
    " TODO: Adapt to new Neovim which-key API
    call which_key#register('\', "b:keymaps")
endif

let b:match_words = '\<if\>:\<elif\>:\<else\>'
let b:match_skip = 'R:^\s*'

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
" MyPy configuration, do not follow imports
let g:ale_python_mypy_options = '--ignore-missing-imports'
" Linters
let b:ale_linters = ['flake8', 'mypy']
" Fixers
" Consider enabling 'reorder-python-imports' if https://github.com/asottile/reorder_python_imports/issues/66 is solved
let b:ale_fixers = ['black']

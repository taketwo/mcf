let maplocalleader = '\'

runtime! syntax/gtest.vim

setlocal foldmethod=syntax
setlocal foldlevel=100

inoremap <buffer> .. ->

" Select until the underscore and substitute
nnoremap <LocalLeader>- vt_s

nnoremap <LocalLeader>p :pythonx TogglePrivate()<CR>
pythonx << EOF
def TogglePrivate():
    import vim
    word = vim.eval('expand("<cword>")')
    if word[-1] == '_':
      converted = word[:-1]
    else:
      converted = word + '_'
    vim.command('normal! viws{}'.format(converted))
    print('{} → {}'.format(word, converted))
EOF

nnoremap <LocalLeader>. :pythonx TogglePointer()<CR>
pythonx << EOF
def TogglePointer():
    import vim
    vim.command("let pos = getpos('.')")
    vim.command('normal! el')
    row, col = vim.current.window.cursor
    line = vim.current.buffer[row - 1]
    vim.command("call setpos('.', pos)")
    if line[col] == '.':
        vim.current.buffer[row - 1] = line[:col] + '->' + line[col + 1:]
        print('. → ->')
    elif line[col:col + 2] == '->':
        vim.current.buffer[row - 1] = line[:col] + '.' + line[col + 2:]
        print('-> → .')
EOF

nnoremap <LocalLeader>a viwbvi&<Esc>

nnoremap <LocalLeader>c :pythonx ToggleCase()<CR>
pythonx << EOF
def ToggleCase():
    from os.path import expanduser, join
    import sys; sys.path.append(join(expanduser("~"), ".mcf/scripts/library"))
    import conversions
    import vim
    word = vim.eval('expand("<cword>")')
    if conversions.is_camelcase(word):
        converted = conversions.camelcase_to_snakecase(word)
    elif conversions.is_snakecase(word):
        converted = conversions.to_camelcase(word)
    else:
        return
    vim.command('normal! viws{}'.format(converted))
    print('{} → {}'.format(word, converted))
EOF

" Remove function call
" When on a function name, removes the name and the parens around its arguments
nnoremap <LocalLeader>f diwds()

" Surround with function call
" Adds braces around selected text and puts cursor in front of them to input
" the function name
vnoremap <LocalLeader>f c(<C-R>")<ESC>%i

" Delete until next closing brace
nnoremap <LocalLeader>0 dt)

" Create a block below and put cursor inside it
inoremap <LocalLeader>] <C-o>o{<CR>}<C-o>O

" Append semicolon to the end of line
nnoremap <buffer> <silent> <LocalLeader>; :call AppendSemicolon()<CR>
inoremap <buffer> <silent> <LocalLeader>; <C-o>:call AppendSemicolon()<CR>

if !has("nvim")
    " YCM mappings, only in Vim
    nnoremap <buffer> gf :YcmCompleter GoToInclude<CR>
    nnoremap <buffer> gD :YcmCompleter GoTo<CR>
    nnoremap <buffer> gd :YcmCompleter GoToImprecise<CR>
    nnoremap <silent> <LocalLeader>f :YcmCompleter FixIt<CR>
    nnoremap <silent> <LocalLeader>t :YcmCompleter GetType<CR>
    nnoremap <silent> <LocalLeader>T :YcmCompleter GetTypeImprecise<CR>
    nnoremap <silent> <LocalLeader>d :YcmCompleter GetDoc<CR>
    nnoremap <silent> <LocalLeader>D :YcmCompleter GetDocImprecise<CR>
endif

" ALE config
let b:ale_enabled = 1
" Linters
let b:ale_linters = [ 'cppcheck' ]
let g:ale_cpp_cppcheck_options = '--enable=all --project=build/compile_commands.json --inline-suppr'
" Setup clang-tidy fixer (pick the newest version)
for v in ['5.0', '4.0', '3.8', '3.6']
    let e = 'clang-tidy-' . v
    if executable(e)
        let g:ale_cpp_clangtidy_executable = e
        break
    endif
endfor
" Fixers
let b:ale_fixers = [ 'clang-format' ]
" Setup clang-format fixer (pick the newest version)
for v in ['9.0', '8.0', '7.0', '6.0', '5.0', '4.0', '3.8', '3.6']
    let e = 'clang-format-' . v
    if executable(e)
        let g:ale_c_clangformat_executable = e
        break
    endif
endfor

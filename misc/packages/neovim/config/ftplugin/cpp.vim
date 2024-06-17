let maplocalleader = '\'

runtime! syntax/gtest.vim

setlocal foldmethod=syntax
setlocal foldlevel=100

lua << EOF
require('which-key').register({
    ['..'] = { '->', 'which_key_ignore' },
    ['...'] = { '...', 'which_key_ignore' },
}, { mode = 'i', buffer = 0 })
EOF

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

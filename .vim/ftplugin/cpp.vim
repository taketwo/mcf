let maplocalleader = '\'
inoremap <buffer> .. ->

" Append semicolon to the end of the line
nnoremap <LocalLeader>; A;<Esc>

" Select until the underscore and substitute
nnoremap <LocalLeader>- vt_s

nnoremap <LocalLeader>p :python TogglePrivate()<CR>
python << EOF
def TogglePrivate():
    import vim
    word = vim.eval('expand("<cword>")')
    if word[-1] == '_':
      converted = word[:-1]
    else:
      converted = word + '_'
    vim.command('normal! viws%s' % converted)
    print '%s → %s' % (word, converted)
EOF

nnoremap <LocalLeader>c :python ToggleCamelCase()<CR>
python << EOF
def ToggleCamelCase():
    from os.path import expanduser, join
    import sys; sys.path.append(join(expanduser("~"), ".mcf/scripts/library"))
    import conversions
    import vim
    word = vim.eval('expand("<cword>")')
    if conversions.is_camelcase(word):
        converted = conversions.camelcase_to_snakecase(word)
    elif conversions.is_snakecase(word):
        converted = conversions.snakecase_to_camelcase(word, True)
    else:
        return
    vim.command('normal! viws%s' % converted)
    print '%s → %s' % (word, converted)
EOF

" Remove function call
" When on a function name, removes the name and the parens around its arguments
nnoremap <LocalLeader>f diwds()

" Delete until next closing brace
nnoremap <LocalLeader>0 dt)

let maplocalleader = '\'
inoremap <buffer> .. ->

" Append semicolon to the end of the line
nnoremap <LocalLeader>; A;<Esc>

nnoremap <LocalLeader>p :call TogglePrivate()<CR>
function! TogglePrivate()
  normal! e
  if getline('.')[col('.')-1] == "_"
    normal! x
  else
    normal! a_
  endif
endfunction

nnoremap <LocalLeader>c :python ToggleCamelCase()<CR>
python << EOF
def ToggleCamelCase():
    from os.path import expanduser, join
    import sys; sys.path.append(join(expanduser("~"), ".mcf/scripts/library"))
    import conversions
    import vim, string
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

" Remove function call: when on a function name,
" removes the name and the parens around its arguments.
nmap <LocalLeader>f diwds()

" Delete until next closing brace
nnoremap <LocalLeader>0 dt)

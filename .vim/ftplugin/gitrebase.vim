nnoremap <C-a> :python NextCommand()<CR>
nnoremap <C-x> :python NextCommand(-1)<CR>
nnoremap <Space> :python NextCommand()<CR>

pythonx << EOF
def NextCommand(delta=1):
    import vim
    CMDS = ['pick', 'reword', 'edit', 'squash', 'fixup']
    word = vim.eval('expand("<cword>")')
    try:
        i = (CMDS.index(word) + delta) % len(CMDS)
        vim.command('normal! viws%s' % CMDS[i])
    except:
        return
EOF

" Change command by pressing "P", "R", etc.
for cmd in ['pick', 'reword', 'edit', 'squash', 'fixup']
    execute 'nnoremap '.toupper(cmd[0]).' :call <SID>ChangeCommand("'.cmd.'")<CR>'
    execute 'vnoremap '.toupper(cmd[0]).' :call <SID>ChangeCommand("'.cmd.'")<CR>'
endfor

function! s:ChangeCommand(command)
    execute 's/^\w\+ /'.a:command.' /'
endfunction

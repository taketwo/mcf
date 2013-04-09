nnoremap <C-a> :python NextCommand()<CR>
nnoremap <C-x> :python NextCommand(-1)<CR>
python << EOF
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

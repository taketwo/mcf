let maplocalleader = ' '

nnoremap <buffer> gd :YcmCompleter GoToDefinition<CR>
nnoremap <buffer> gr :YcmCompleter GoToReferences<CR>
nnoremap <silent> <LocalLeader>r :YcmCompleter RefactorRename<CR>
nnoremap <silent> <LocalLeader>t :YcmCompleter GetType<CR>
nnoremap <silent> <LocalLeader>d :YcmCompleter GetDoc<CR>

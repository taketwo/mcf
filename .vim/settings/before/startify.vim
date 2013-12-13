let g:startify_list_order = [
    \ ['   Sessions:'],
    \ 'sessions',
    \ ['   Recently edited files'],
    \ 'files',
    \ ['   Bookmarks:'],
    \ 'bookmarks',
    \ ['   Directory:'],
    \ 'dir',
    \ ]
let g:startify_bookmarks = [ '~/.bashrc', '~/.vim/plugins.vim' ]
let g:startify_files_number = 10
let g:startify_session_detection = 1
let g:startify_session_persistence = 0
let g:startify_change_to_dir = 0
let g:startify_skiplist = [
    \ 'COMMIT_EDITMSG',
    \ $VIMRUNTIME .'/doc',
    \ 'bundle/.*/doc',
    \ ]
let g:ctrlp_reuse_window = 'startify'

if executable('ag')
    " Use ag in CtrlP for listing files, lightning fast and respects .gitignore
    let g:ctrlp_user_command = 'ag %s --files-with-matches --hidden -g "" --ignore "\.git$\|\.hg$\|\.svn$\|\.pyc$"'
    " ag is fast enough that CtrlP doesn't need to cache
    let g:ctrlp_use_caching = 0
else
    " Custom file listing when inside git repository
    let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']
endif

if has('python')
    let g:ctrlp_match_func = {'match': 'pymatcher#PyMatch'}
endif

" Show dotfiles
let g:ctrlp_show_hidden = 1

" Set no file limit
let g:ctrlp_max_files = 0

" Do not clear filenames cache
let g:ctrlp_clear_cache_on_exit = 0

" Set delay to prevent extra search
let g:ctrlp_lazy_update = 100

" Ignore version control folders and other stuff
let g:ctrlp_custom_ignore = {
    \ 'dir':  '\v[\/]\.(git|hg|svn)$',
    \ 'file': '\v\.(exe|so|dll)$',
    \ 'link': 'SOME_BAD_SYMBOLIC_LINKS',
    \ }

" Number of items to display
let g:ctrlp_max_height = 15

" Default to filename searches
let g:ctrlp_by_filename = 1

" We don't want to use Ctrl-p as the mapping
let g:ctrlp_map = '<Leader>..'

nnoremap <Leader>.. :CtrlP<CR>

" Idea from:
" http://www.charlietanksley.net/blog/blog/2011/10/18/vim-navigation-with-lustyexplorer-and-lustyjuggler/
" Open CtrlP starting from a particular path, making it much more likely to find the correct thing first.
nnoremap <Leader>.s :CtrlP src<CR>
nnoremap <Leader>.h :CtrlP include<CR>
nnoremap <Leader>.i :CtrlP impl<CR>
nnoremap <Leader>.b :CtrlPBuffer<CR>
nnoremap <Leader>.t :CtrlPBufTag<CR>
nnoremap <Leader>.T :CtrlPBufTagAll<CR>
nnoremap <Leader>.l :CtrlPLine<CR>

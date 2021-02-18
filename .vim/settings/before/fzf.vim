let g:fzf_command_prefix = 'Fzf'

nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

if !has('nvim')
    " In Neovim these are handled by telescope plugin
    nnoremap <Leader>.. :FzfFiles<CR>
    nnoremap <Leader>.b :FzfBuffers<CR>
endif

nnoremap <Leader>.t :FzfBTags<CR>
nnoremap <Leader>.T :FzfTags<CR>
nnoremap <Leader>.c :FzfBCommits<CR>
nnoremap <Leader>.h :FzfHelptags<CR>
nnoremap <Leader>.s :FzfSnippets<CR>
nnoremap <Leader>.r :FzfRg<CR>

nnoremap <Space> :FzfBuffers<CR>

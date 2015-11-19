let g:fzf_command_prefix = 'Fzf'

nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

nnoremap <Leader>.. :FzfFiles<CR>
nnoremap <Leader>.b :FzfBuffers<CR>
nnoremap <Leader>.t :FzfBTags<CR>
nnoremap <Leader>.T :FzfTags<CR>
nnoremap <Leader>.c :FzfBCommits<CR>

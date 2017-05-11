setlocal wrap linebreak nolist

nnoremap <buffer> <Space><Space> gq}

abbr ie \ie
abbr eg \eg

call textobj#user#plugin('latex', {
\  'math-a': {
\     'pattern': '[$][^$]*[$]',
\     'select': 'am',
\   },
\  'math-i': {
\     'pattern': '[$]\zs[^$]*\ze[$]',
\     'select': 'im',
\   }
\ })

" Steal F3 from Tagbar
" Vimtex TOC serves the same purpose, but is more powerful
nnoremap <F3> :VimtexTocToggle<CR>

" Remap up/down motion commands to move by screen lines
nnoremap c gk
nnoremap t gj
xnoremap c gk
xnoremap t gj

" Remap go to the beginning and the end of line to work with screen lines
nnoremap - g$
vnoremap - g$
nnoremap _ g^
vnoremap _ g^

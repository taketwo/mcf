setlocal wrap nolist

nnoremap <buffer> <Space><Space> gq}

abbr ie \ie
abbr eg \eg

" Steal F3 from Tagbar
" Vimtex TOC serves the same purpose, but is more powerful
nnoremap <F3> :VimtexTocToggle<CR>

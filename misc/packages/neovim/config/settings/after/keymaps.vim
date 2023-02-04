if exists('g:vimspector_enable_mappings')
  finish
endif

" Misc {{{

" Make Y consistent with C and D

nnoremap Y y$

" Backspace proxy

inoremap <C-d> <BS>

" }}}

" Floaterm <F6>, set in floaterm.vim settings file

" Enable spell checking
nnoremap <F7> :setlocal spell! spelllang=en_us<CR>

" Run make with F9
nnoremap <F9> :MakeTarget<CR>
inoremap <F9> <C-o>:MakeTarget<CR>
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" Some keys that are still free
"
" $ ^ F5 F10

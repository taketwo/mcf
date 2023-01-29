if exists('g:vimspector_enable_mappings')
  finish
endif

" Move around {{{

" Jump over location list items (populated by LSP/ALE) with wrapping
command! Lnext try | lnext | catch | try | lfirst | catch | endtry | endtry
command! Lprev try | lprev | catch | try | llast | catch | endtry | endtry
nnoremap > :Lnext<CR>
nnoremap < :Lprev<CR>

" }}}
" Misc {{{

" Make Y consistent with C and D

nnoremap Y y$

" Backspace proxy

inoremap <C-d> <BS>

" }}}

" ALEFix
nnoremap <F2> :ALEFix<CR>

" Floaterm <F6>, set in floaterm.vim settings file

" Enable spell checking
nnoremap <F7> :setlocal spell! spelllang=en_us<CR>

" Run make with F9
nnoremap <F9> :MakeTarget<CR>
inoremap <F9> <C-o>:MakeTarget<CR>
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" PearTree

imap <C-s> <Plug>(PearTreeJump)

" AnyJump
nnoremap <F3> :AnyJump<CR>

" Some keys that are still free
"
" $ ^ F5 F10

if exists('g:vimspector_enable_mappings')
  finish
endif

let g:mcf#keymaps = {}

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

" Replace word with what's in the yank buffer

nnoremap <Leader>rw "_diwhp

" Now that c/C are used for navigation, utilize k/K for the same purpose

nnoremap k c
nnoremap K C

" Backspace proxy

inoremap <C-d> <BS>

" Copy short/long filename to clipboard

let g:mcf#keymaps.f = { "name" : "+filename" }
let g:mcf#keymaps.f.s = "copy-short"
let g:mcf#keymaps.f.l = "copy-long"
nnoremap <Leader>fs :let @+=expand("%")<CR>
nnoremap <Leader>fl :let @+=expand("%:p")<CR>

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

" Create WhichKey menu
if exists("which_key#register")
  call which_key#register(',', "g:mcf#keymaps")
endif

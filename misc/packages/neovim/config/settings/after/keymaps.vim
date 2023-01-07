if exists('g:vimspector_enable_mappings')
  finish
endif

let g:mcf#keymaps = {}

" Move around {{{

" Accelerated up and down (scrolling)

nnoremap C <PageUp>
nnoremap T <PageDown>

" Go to the beginning and the end of line with _/-

nnoremap - $
vnoremap - $
nnoremap _ ^
vnoremap _ ^

" Jump over location list items (populated by LSP/ALE) with wrapping
command! Lnext try | lnext | catch | try | lfirst | catch | endtry | endtry
command! Lprev try | lprev | catch | try | llast | catch | endtry | endtry
nnoremap > :Lnext<CR>
nnoremap < :Lprev<CR>

" }}}
" Push text around {{{

" Move lines up/down

nnoremap <C-t> :m .+1<CR>
nnoremap <C-c> :m .-2<CR>
inoremap <C-t> <Esc>:m .+1<CR>==gi
inoremap <C-c> <Esc>:m .-2<CR>==gi
vnoremap <C-t> :m '>+1<CR>gv=gv
vnoremap <C-c> :m '<-2<CR>gv=gv

" Change indentation (i.e. move lines left/right)

nnoremap <C-h> :<<CR>
nnoremap <C-n> :><CR>
inoremap <C-h> <C-d>
inoremap <C-n> <C-t>
vnoremap <C-h> :<<CR>gv
vnoremap <C-n> :><CR>gv

" }}}
" Window management {{{

" Move between split windows similarly to normal motion

" with <C-w> prefix
noremap <C-w><C-n> <C-w>l
noremap <C-w><C-c> <C-w>k
noremap <C-w><C-t> <C-w>j
" ... and also with Alt- prefix (handled by vim-tmux-navigator plugin)
nnoremap <silent> <M-n> :TmuxNavigateRight<CR>
nnoremap <silent> <M-c> :TmuxNavigateUp<CR>
nnoremap <silent> <M-t> :TmuxNavigateDown<CR>
nnoremap <silent> <M-h> :TmuxNavigateLeft<CR>
nnoremap <silent> <M-Space> :TmuxNavigatePrevious<cr>

" Resize windows with Alt+arrow
nnoremap <M-Up> <C-w>+
nnoremap <M-Down> <C-w>-
nnoremap <M-Right> <C-w>>
nnoremap <M-Left> <C-w><

" }}}
" Misc {{{

" Make Y consistent with C and D

nnoremap Y y$

" Copy entire word even if the cursor is halfway inside the word

nmap <Leader>yw yiww
nmap <Leader>y( yi(h%
nmap <Leader>y" yi"h%

" Replace word with what's in the yank buffer

nnoremap <Leader>rw "_diwhp

" Now that c/C are used for navigation, utilize k/K for the same purpose

nnoremap k c
nnoremap K C

" Insert newline below, but stay on the same spot

nnoremap <CR> :call append(line('.'), '')<CR>

" Insert newline above, but stay on the same spot

nnoremap <NL> :call append(line('.')-1, '')<CR>

" Duplicate current line

nnoremap <Leader>d :t.<CR>

" Backspace proxy

inoremap <C-d> <BS>

" Join line (because J is used by 'vim-sneak')

nnoremap <C-J> J

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

" Run last Vimux command with F8
nnoremap <F8> :write<CR>:VimuxRunLastCommand<CR>
nnoremap <F8><F8> :write<CR>:VimuxRunLastCommand<CR>:VimuxZoomRunner<CR>
inoremap <F8> <C-o>:write<CR><C-o>:VimuxRunLastCommand<CR>

" Run make with F9
nnoremap <F9> :MakeTarget<CR>
inoremap <F9> <C-o>:MakeTarget<CR>
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

  " Change inside surroundings

  let g:mcf#keymaps.k = { "name" : "+change-surrounding" }
  let g:mcf#keymaps.k.i = "change-inside"
  let g:mcf#keymaps.k.a = "change-around"
  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>

  " DelimitMate

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

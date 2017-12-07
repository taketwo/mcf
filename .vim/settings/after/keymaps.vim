" Enter command mode with semicolon

  nnoremap ; :

" Move around {{{

" Left, right, up, and down
"
"       c
"     h t n

" Mappings for Normal mode

nnoremap c k
nnoremap t j
nnoremap n l

" Mappings for Visual (but not Select!) mode

xnoremap c k
xnoremap t j
xnoremap n l

" Accelerated up and down (scrolling)

nnoremap C <PageUp>
nnoremap T <PageDown>

" Go to the beginning and the end of line with _/-

nnoremap - $
vnoremap - $
nnoremap _ ^
vnoremap _ ^

" Navigate between buffers with N/H

noremap N :bnext<CR>
noremap H :bprev<CR>

" }}}
" Push text around {{{

" Move lines up/down

nnoremap <C-t> :m+<CR>==
nnoremap <C-c> :m-2<CR>==
inoremap <C-t> <Esc>:m+<CR>==gi
inoremap <C-c> <Esc>:m-2<CR>==gi

" Change indentation (i.e. move lines left/right)

nnoremap <C-h> :<<CR>
nnoremap <C-n> :><CR>
inoremap <C-h> <C-d>
inoremap <C-n> <C-t>
vnoremap <C-h> :<<CR>gv
vnoremap <C-n> :><CR>gv

" }}}
" Surroundings {{{

  " Surround a word with "quotes"
  map <Leader>" <Plug>Ysurroundiw"
  vnoremap <Leader>" c"<C-R>""<ESC>

  " Surround a word with 'single quotes'
  map <Leader>' <Plug>Ysurroundiw'
  vnoremap <Leader>' c'<C-R>"'<ESC>

  " Surround a word with (parenthesis)
  " Difference is where the cursor ends up after surrounding
  map <Leader>( <Plug>Ysurroundiw)
  map <Leader>) <Plug>Ysurroundiw)%
  vnoremap <Leader>( c(<C-R>")<ESC>%
  vnoremap <Leader>) c(<C-R>")<ESC>

  " Surround a word with [brackets]
  " Difference is where the cursor ends up after surrounding
  map <Leader>[ <Plug>Ysurroundiw]
  map <Leader>] <Plug>Ysurroundiw]%
  vnoremap <Leader>[ c[<C-R>"]<ESC>%
  vnoremap <Leader>] c[<C-R>"]<ESC>

  " Surround a word with {braces}
  " Difference is where the cursor ends up after surrounding
  map <Leader>{ <Plug>Ysurroundiw}
  map <Leader>} <Plug>Ysurroundiw}%
  vnoremap <Leader>{ c{<C-R>"}<ESC>%
  vnoremap <Leader>} c{<C-R>"}<ESC>

  " Surround a word with <braces>
  " Difference is where the cursor ends up after surrounding
  map <Leader>< <Plug>Ysurroundiw>
  map <Leader>> <Plug>Ysurroundiw>%
  vnoremap <Leader>< c<<C-R>"><ESC>%
  vnoremap <Leader>> c<<C-R>"><ESC>

  " Surround a word with *asterisks*
  map <Leader>* <Plug>Ysurroundiw*
  vnoremap <Leader>* c*<C-R>"*<ESC>

  " Surround a word with `backticks` (useful for Markdown)
  map <Leader>` <Plug>Ysurroundiw`
  vnoremap <Leader>` c`<C-R>"`<ESC>

  " Surround a word with $dollars$ (useful for typesetting math in LaTeX)
  map <Leader>$ <Plug>Ysurroundiw$
  vnoremap <Leader>$ c$<C-R>"$<ESC>

  " Delete surrounding
  nmap ds <Plug>Dsurround

  " Change surrounding
  nmap ks <Plug>Csurround

" }}}
" Window management {{{

" Move between split windows similarly to normal motion

noremap <C-w><C-n> <C-w>l
noremap <C-w><C-c> <C-w>k
noremap <C-w><C-t> <C-w>j

" Resize windows with Alt+arrow

nnoremap <Up> <C-w>+
nnoremap [1;3A <C-w>+
nnoremap <Down> <C-w>-
nnoremap [1;3B <C-w>-
nnoremap <Right> <C-w>>
nnoremap [1;3C <C-w>>
nnoremap <Left> <C-w><
nnoremap [1;3D <C-w><

" }}}
" Search {{{

" Reset search pattern

nnoremap <Leader>/ :let @/ = ""<CR>

" Seek through search results with l/L (instead of used n/N)

nnoremap l n
nnoremap L N

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

" Enable spell checking

nnoremap <F7> :setlocal spell! spelllang=en_us<CR>

" Pasting
set pastetoggle=<S-F5>
inoremap <F5> <C-o>:call PasteFromClipboard()<CR>

" Close the current buffer with F11

nnoremap <F11> :BW<CR>

" Join line (because J is used by 'vim-sneak')

nnoremap <C-J> J

" Run make with F9

nnoremap <F9> :MakeTarget<CR>
inoremap <F9> <C-o>:MakeTarget<CR>
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" Jump to next line in location list (useful for Syntastic)

nnoremap >l :lnext<CR>
nnoremap <l :lprev<CR>

" Copy short/long filename to clipboard

nnoremap <Leader>fs :!xsel --clipboard <<< '%'<CR>
nnoremap <Leader>fl :!xsel --clipboard <<< '%:p'<CR>

" ========================== Shortcuts for plugins ========================= "

  " NERDTree

  noremap <F12> :NERDTreeMirrorToggle<CR>

  " ListToggle

  nnoremap <F10> :QToggle<CR>

  " Tagbar

  nnoremap <F3> :TagbarToggle<CR>

  " Syntastic

  noremap <F6> :w<CR>:SyntasticCheck<CR>
  noremap <S-F6> :SyntasticToggleMode<CR>

  " Gundo

  nnoremap <F4> :GundoToggle<CR>

  " Change inside surroundings

  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>

  " DelimitMate

  imap <C-s> <Plug>delimitMateS-Tab

  " Sideways

  nnoremap <Leader>sh :SidewaysLeft<CR>
  nnoremap <Leader>sn :SidewaysRight<CR>
  nnoremap <Leader>sH :SidewaysJumpLeft<CR>
  nnoremap <Leader>sN :SidewaysJumpRight<CR>

" Some keys that are still free
"
" C-f, C-b : scroll up/down by default

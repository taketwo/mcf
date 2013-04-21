" Enter command mode with semicolon

  nnoremap ; :
  nnoremap : <Esc> " temporally disable : to get rid of the habit

  " Experimental
  call arpeggio#load()
  "Arpeggio inoremap uh <Esc>
  "Arpeggio inoremap nh <Esc>
  inoremap hh <Esc>
  inoremap uu <Esc>

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

  " Surround a word with `backticks`
  map <Leader>` <Plug>Ysurroundiw`
  vnoremap <Leader>` c`<C-R>"`<ESC>

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

  nnoremap [1;3A <C-w>+
  nnoremap [1;3B <C-w>-
  nnoremap [1;3C <C-w>>
  nnoremap [1;3D <C-w><

" }}}
" Search {{{

  " Reset search pattern

  nnoremap <Leader>/ :let @/ = ""<CR>

  " Seek through search results with l/L (instead of used n/N)

  nnoremap l n
  nnoremap L N

  " Use j instead of t in normal mode to (j)ump to

  nnoremap j t

" }}}
" Misc {{{

  " Copy entire word even if the cursor is halfway inside the word

  nnoremap <Leader>yw yiww

  " Replace word with what's in the yank buffer

  nnoremap <Leader>rw "_diwhp

  " Make Y consistent with C and D

  nnoremap Y y$

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

  " Semicolon at end of line by typing ;;
  inoremap ;; <C-o>A;<Esc>

  " Enable spell checking
  nnoremap <F7> :setlocal spell! spelllang=en_us<CR>

  set pastetoggle=<F4>

  " Close the current buffer with F11
  nnoremap <F11> :BW<CR>

  " Split line (opposite to S-J joining line)
  nnoremap <silent> <C-J> gEa<CR><Esc>ew

  " Run make with F9
  nnoremap <F9> :make<CR>
  inoremap <F9> <C-o>:make<CR>
  autocmd QuickFixCmdPost [^l]* nested cwindow
  autocmd QuickFixCmdPost    l* nested lwindow

  " Jump to next line in location list (useful for Syntastic)
  nnoremap <Leader>e :ll<CR>
  nnoremap <Leader>E :lp<CR>

  " Copy short/long filename to clipboard
  nnoremap <Leader>fs :!xsel --clipboard <<< '%'<CR>
  nnoremap <Leader>fl :!xsel --clipboard <<< '%:p'<CR>

" ========================== Shortcuts for plugins ========================= "

  " NERDTree

  noremap <F12> :NERDTreeMirrorToggle<CR>

  " Tagbar

  nnoremap <F3> :TagbarToggle<CR>

  " Syntastic

  noremap <F6> :w<CR>:SyntasticCheck<CR>

  " Minibufexplorer

  noremap <F2> :TMiniBufExplorer<CR>
  noremap <Leader>n :MBEbn<CR>
  noremap <Leader>h :MBEbp<CR>
  noremap N :MBEbn<CR>
  noremap H :MBEbp<CR>
  for i in range(1,9)
    exec "noremap <Leader>".i." :b".i."<CR>"
  endfor

  " Change inside surroundings

  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>

  " DelimitMate

  imap <C-l> <Plug>delimitMateS-Tab

  " UltiSnip

  " Temporary hack - enable autocompletion in visual mode
  Arpeggio xmap wv :call UltiSnips_SaveLastVisualSelection()<CR>gvs

  " Sideways

  nnoremap <Leader>sh :SidewaysLeft<CR>
  nnoremap <Leader>sn :SidewaysRight<CR>

" ============================== Super combos ============================== "

  " (E)xit input mode, (S)ave, (M)ake
  Arpeggio inoremap esm <Esc>:w<CR>:make<CR>

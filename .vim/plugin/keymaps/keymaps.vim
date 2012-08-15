" Change leader to a comma because the backslash is too far away
" That means all \x commands turn into ,x

  let mapleader=','

" Enter command mode with semicolon

  nnoremap ; :
  nnoremap : <Esc> " temporally disable : to get rid of the habit

  " Experimental
  call arpeggio#load()
  Arpeggio inoremap uh <Esc>
  Arpeggio inoremap nh <Esc>

" ============================ Move text around ============================ "

  " Left, right, up, and down

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

  " Note: j is now unused, find something for it!

  " Accelerated up and down (scrolling)

  nnoremap C <PageUp>
  nnoremap T <PageDown>

  " Go to the beginning and the end of line with _/-

  nnoremap - $
  nnoremap _ ^

" ============================ Push text around ============================ "

  " Move lines up/down

  nnoremap <C-t> :m+<CR>==
  nnoremap <C-c> :m-2<CR>==
  inoremap <C-t> <Esc>:m+<CR>==gi
  inoremap <C-c> <Esc>:m-2<CR>==gi

  " Change indentation (i.e. move lines left/right)

  nnoremap <C-h> :<<CR>
  nnoremap <C-n> :><CR>
  vnoremap <C-h> :<<CR>gv
  vnoremap <C-n> :><CR>gv
  "inoremap <C-h> <C-d>
  "inoremap <C-n> <C-t>

" ============================ Text manipulation =========================== "

  " Surround a word with "quotes"
  map <Leader>" <Plug>Ysurroundiw"
  vnoremap <Leader>" c"<C-R>""<ESC>

  " Surround a word with 'single quotes'
  map <Leader>' <Plug>Ysurroundiw'
  vnoremap <Leader>' c'<C-R>"'<ESC>

  " Surround a word with (parens)
  map <Leader>( <Plug>Ysurroundiw)
  vnoremap <Leader>( c(<C-R>")<ESC>

  " Surround a word with [brackets]
  map <Leader>[ <Plug>Ysurroundiw]
  vnoremap <Leader>[ c[<C-R>"]<ESC>

  " Surround a word with {braces}
  map <Leader>{ <Plug>Ysurroundiw}
  vnoremap <Leader>{ c{<C-R>"}<ESC>

  " Surround a word with <braces>
  map <Leader>< <Plug>Ysurroundiw>
  vnoremap <Leader>< c<<C-R>"><ESC>

" ============================ Window management =========================== "

  " Move between split windows similarlay to normal motion

  noremap <C-w><C-n> <C-w>l
  noremap <C-w><C-c> <C-w>k
  noremap <C-w><C-t> <C-w>j

  " Resize windows with Alt+arrow

  nnoremap [1;3A <C-w>+
  nnoremap [1;3B <C-w>-
  nnoremap [1;3C <C-w>>
  nnoremap [1;3D <C-w><

" ================================== Misc ================================== "

  " Copy entire word even if the cursor is halfway inside the word

  nnoremap <Leader>cw yiww

  " Replace word with what's in the yank buffer

  nnoremap <Leader>rw "_diwhp

  " Make Y consistent with C and D

  nnoremap Y y$

" Seek through search results with l/L (instead of used n/N)

  nnoremap l n
  nnoremap L N

" Now that c/C are used for navigation, utilize k/K for the same purpose

  nnoremap k c
  nnoremap K C

" Reset search pattern

  nnoremap <Leader>/ :let @/ = ""<CR>

" Insert newline below, but stay on the same spot
  nnoremap <CR> :call append(line('.'), '')<CR>
" Insert newline above, but stay on the same spot
  nnoremap <NL> :call append(line('.')-1, '')<CR>

" Duplicate current line
  nnoremap <Leader>d :t.<CR>

" Enable spell checking
  nnoremap <F7> :setlocal spell! spelllang=en_us<CR>

  "set pastetoggle=<Leader>pt "not sure about this one yet

  nnoremap <C-s> :w<CR>

  nnoremap <F9> :make<CR>
  autocmd QuickFixCmdPost [^l]* nested cwindow
  autocmd QuickFixCmdPost    l* nested lwindow

" Jump to next line in location list (useful for Syntactic)
  nnoremap <Leader>e :lne<CR>
  nnoremap <Leader>E :lp<CR>

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
  for i in range(1,9)
    exec "noremap <Leader>".i." :b".i."<CR>"
  endfor

  " Change inside surroundings

  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>

  " DelimitMate

  imap <C-l> <Plug>delimitMateS-Tab
  Arpeggio imap cr <Plug>delimitMateS-Tab

  " UltiSnip
  " Temporary hack - enable autocompletion in visual mode
  Arpeggio xmap wv :call UltiSnips_SaveLastVisualSelection()<CR>gvs

" ============================== Super combos ============================== "

  " (E)xit input mode, (S)ave, (M)ake
  Arpeggio inoremap esm <Esc>:w<CR>:make<CR>

  " (Y)ank current line, (S)witch to left window, (P)aste
  Arpeggio nnoremap ysp yy<C-w><C-h>P

" ============================ Special commands ============================ "

  " Special command to to write a file as sudo (w!!)

  cmap w!! w !sudo tee % >/dev/null

  " This is certainly a bag place, but for the time being...
  " Highlight all instances of word under cursor, when idle.
  nnoremap <Leader>wh :if AutoHighlightToggle()<Bar>set hls<Bar>endif<CR>
  function! AutoHighlightToggle()
    let @/ = ''
    if exists('#auto_highlight')
      au! auto_highlight
      augroup! auto_highlight
        setl updatetime=4000
        echo 'Highlight current word: off'
        return 0
    else
      augroup auto_highlight
        au!
        au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
      augroup end
      setl updatetime=500
      echo 'Highlight current word: ON'
      return 1
    endif
  endfunction

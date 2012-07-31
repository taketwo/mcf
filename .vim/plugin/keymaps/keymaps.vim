" Change leader to a comma because the backslash is too far away
" That means all \x commands turn into ,x

  let mapleader=','

" Enter command mode with semicolon

  nnoremap ; :
  nnoremap : <Esc> " temporally disable : to get rid of the habit

" Move around with Dvorak layout:
"
"       c
"     h t n
"
" Note: j, and l are now unused, find something for them!
" Mappings for Normal mode
  nnoremap c k
  nnoremap t j
  nnoremap n l
" Mappings for Visual (but not Select!) mode
  xnoremap c k
  xnoremap t j
  xnoremap n l

" Accelerated up and down

  nnoremap C <PageUp>
  nnoremap T <PageDown>

" Move lines up/down

  nnoremap <C-t> :m+<CR>==
  nnoremap <C-c> :m-2<CR>==
  inoremap <C-t> <Esc>:m+<CR>==gi
  inoremap <C-c> <Esc>:m-2<CR>==gi

" Go to the beginning and the end of line with _/-

  nnoremap - $
  nnoremap _ ^

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

" Quick switch between windows with N

  nnoremap N <C-w><C-w>

" Change indentation
  nnoremap <C-h> :<<CR>go
  nnoremap <C-n> :><CR>
  vnoremap <C-h> :<<CR>gv
  vnoremap <C-n> :><CR>gv
  inoremap <C-h> <C-d>
  inoremap <C-n> <C-t>

  nnoremap <Leader>s :setlocal spell! spelllang=en_us<CR>
  "set pastetoggle=<Leader>pt "not sure about this one yet

" ================= Shortcuts for plugins ====================================
" ================= NERDTree =================================================
  noremap <Leader>n :NERDTreeMirrorToggle<CR>
" ================= Change inside surroundings ===============================
  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>
" ================= Tagbag ===================================================
  nnoremap <Leader>t :TagbarToggle<CR>
" ================= DelimitMate ==============================================
  imap <C-l> <Plug>delimitMateS-Tab

" Special command to to write a file as sudo (w!!)
  cmap w!! w !sudo tee % >/dev/null

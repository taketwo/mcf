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

  noremap c k
  noremap t j
  noremap n l

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

" Seek through search results with s/S (instead of used n/N)

  nnoremap s n
  nnoremap S N

" Now that c/C are used for navigation, utilize k/K for the same purpose

  nnoremap k c
  nnoremap K C

" Reset search pattern

  nmap <Leader>/ :let @/ = ""<CR>

" Quickly insert a newline by just hitting enter
  nnoremap <CR> o<Esc>

" Quick switch between windows with N

  nnoremap N <C-w><C-w>

  nmap <Leader>n <C-w><C-w>

  nnoremap <Leader>s :setlocal spell! spelllang=en_us<CR>
" ================= Shortcuts for plugins ====================================
" ================= NERDTree =================================================
" Toggle NERDTree
  noremap <C-n> :NERDTreeMirrorToggle<CR>
" ================= Change inside surroundings ===============================
  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>
" Special command to to write a file as sudo (w!!)
  cmap w!! w !sudo tee % >/dev/null

" Change leader to a comma because the backslash is too far away
" That means all \x commands turn into ,x

  let mapleader=","

" Enter command mode with semicolon

  nnoremap ; :

" Move around in Dvorak layout:
"
"       c
"     h t n
"
" Note: k, j, and l are now unused, find something for them!

  nnoremap c k
  nnoremap t j
  nnoremap n l

" Go to the beginning and the end of line with _/-

  nnoremap - $
  nnoremap _ ^

" Seek through search results with s/S (instead of used n/N)

  nnoremap s n
  nnoremap S N

" Quick switch between windows with N

  nnoremap N <C-w><C-w>

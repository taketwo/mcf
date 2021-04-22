if exists('g:vimspector_enable_mappings')
  finish
endif

let g:mcf#keymaps = {}

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

" Jump over location list items (populated by LSP/ALE/YCM) with wrapping
command! Lnext try | lnext | catch | try | lfirst | catch | endtry | endtry
command! Lprev try | lprev | catch | try | llast | catch | endtry | endtry
nnoremap > :Lnext<CR>
nnoremap < :Lprev<CR>

" }}}
" Push text around {{{

" Move lines up/down

nnoremap <C-t> :m .+1<CR>==
nnoremap <C-c> :m .-2<CR>==
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

" with <C-w> prefix
noremap <C-w><C-n> <C-w>l
noremap <C-w><C-c> <C-w>k
noremap <C-w><C-t> <C-w>j
" ... and also with Alt- prefix (handled by vim-tmux-navigator plugin)
if has('nvim')
  nnoremap <silent> <M-n> :TmuxNavigateRight<CR>
  nnoremap <silent> <M-c> :TmuxNavigateUp<CR>
  nnoremap <silent> <M-t> :TmuxNavigateDown<CR>
  nnoremap <silent> <M-h> :TmuxNavigateLeft<CR>
  nnoremap <silent> <M-Space> :TmuxNavigatePrevious<cr>
else
  nnoremap <silent> n :TmuxNavigateRight<CR>
  nnoremap <silent> c :TmuxNavigateUp<CR>
  nnoremap <silent> t :TmuxNavigateDown<CR>
  nnoremap <silent> h :TmuxNavigateLeft<CR>
  nnoremap <silent> <Space> :TmuxNavigatePrevious<cr>
endif

" Resize windows with Alt+arrow
if has('nvim')
  nnoremap <M-Up> <C-w>+
  nnoremap <M-Down> <C-w>-
  nnoremap <M-Right> <C-w>>
  nnoremap <M-Left> <C-w><
else
  nnoremap <Up> <C-w>+
  nnoremap [1;3A <C-w>+
  nnoremap <Down> <C-w>-
  nnoremap [1;3B <C-w>-
  nnoremap <Right> <C-w>>
  nnoremap [1;3C <C-w>>
  nnoremap <Left> <C-w><
  nnoremap [1;3D <C-w><
endif

" }}}
" Search {{{

" Reset search pattern

let g:mcf#keymaps["/"] = { "name" : "reset-search-pattern" }
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

" Join line (because J is used by 'vim-sneak')

nnoremap <C-J> J

" Copy short/long filename to clipboard

let g:mcf#keymaps.f = { "name" : "+filename" }
let g:mcf#keymaps.f.s = "copy-short"
let g:mcf#keymaps.f.l = "copy-long"
if has('nvim')
    nnoremap <Leader>fs :let @+=expand("%")<CR>
    nnoremap <Leader>fl :let @+=expand("%:p")<CR>
else
    nnoremap <Leader>fs :!xsel --clipboard <<< '%'<CR>
    nnoremap <Leader>fl :!xsel --clipboard <<< '%:p'<CR>
endif

" }}}
" Git / version control {{{

nnoremap <silent> <leader>gc :call magit#show_magit('v')<CR>
nnoremap <silent> <leader>gb :Git blame<CR>
nnoremap <silent> <leader>gl :Telescope git_bcommits<CR>
nnoremap <silent> <leader>gL :Telescope git_commits<CR>
nnoremap <silent> <leader>gs :Telescope git_status<CR>
nnoremap <silent> <leader>ghs :GitGutterStageHunk<CR>
nnoremap <silent> <leader>ghu :GitGutterUndoHunk<CR>
nnoremap <silent> <leader>ghp :GitGutterPreviewHunk<CR>
let g:mcf#keymaps.g = { 'name' : '+git/version-control' }
let g:mcf#keymaps.g.c = 'commit'
let g:mcf#keymaps.g.l = 'log (buffer only)'
let g:mcf#keymaps.g.L = 'log (everything)'
let g:mcf#keymaps.g.b = 'blame'
let g:mcf#keymaps.g.s = 'status'
let g:mcf#keymaps.g.m = 'view commit message'
let g:mcf#keymaps.g.h = { 'name' : '+hunks' }
let g:mcf#keymaps.g.h.s = 'stage'
let g:mcf#keymaps.g.h.u = 'undo'
let g:mcf#keymaps.g.h.p = 'preview'

" }}}
" LSP {{{

if has('nvim')
    nnoremap <silent> <leader>la :Lspsaga code_action<CR>
    nnoremap <silent> <leader>lr :Lspsaga rename<CR>
    nnoremap <silent> <leader>ld :Lspsaga preview_definition<CR>
    let g:mcf#keymaps.l = { 'name' : '+lsp' }
    let g:mcf#keymaps.l.a = 'actions'
    let g:mcf#keymaps.l.r = 'rename'
    let g:mcf#keymaps.l.d = 'definition preview'
endif

" }}}

" ALEFix
nnoremap <F2> :ALEFix<CR>

" Tagbar
nnoremap <F3> :TagbarToggle<CR>

" UndoTree
nnoremap <F4> :UndotreeToggle<CR>

" Pasting
set pastetoggle=<S-F5>
inoremap <F5> <C-o>:call PasteFromClipboard()<CR>
nnoremap <F5> :call PasteFromClipboard()<CR>

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

" Close the current buffer with F11
nnoremap <F11> :BW<CR>

" NERDTree
noremap <F12> :NERDTreeMirrorToggle<CR>

  " Change inside surroundings

  let g:mcf#keymaps.k = { "name" : "+change-surrounding" }
  let g:mcf#keymaps.k.i = "change-inside"
  let g:mcf#keymaps.k.a = "change-around"
  nnoremap <Leader>ki :ChangeInsideSurrounding<CR>
  nnoremap <Leader>ka :ChangeAroundSurrounding<CR>

  " DelimitMate

  imap <C-s> <Plug>(PearTreeJump)

" Sideways
let g:mcf#keymaps["s"] = {
\   "name" : "+sideways"    ,
\   "h"    : ["SidewaysLeft"                 , 'push-left']           ,
\   "n"    : ["SidewaysRight"                , 'push-right']          ,
\   "H"    : ["SidewaysJumpLeft"             , 'jump-left']           ,
\   "N"    : ["SidewaysJumpRight"            , 'jump-right']          ,
\ }

" AnyJump
nnoremap <Tab> :AnyJump<CR>

" Completion-nvim
" Manually trigger completion with Tab
if has("nvim")
    imap <Tab> <Plug>(completion_smart_tab)
    imap <S-Tab> <Plug>(completion_smart_s_tab)
endif

" Some keys that are still free
"
" $ ^ F10

" Create WhichKey menu
call which_key#register(',', "g:mcf#keymaps")

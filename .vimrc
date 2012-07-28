" Use Vim settings, rather then Vi settings (much better!).
" This must be first, because it changes other options as a side effect.
set nocompatible

" =============== Pathogen Initialization ===============
" This loads all the plugins in ~/.vim/bundle
" Use tpope's pathogen plugin to manage all other plugins

  call pathogen#infect()
  call pathogen#helptags()

" ================ General Config ====================

  set number                      " Line numbers are good
  set backspace=indent,eol,start  " Allow backspace in insert mode
  set history=1000                " Store lots of :cmdline history
  set showcmd                     " Show incomplete cmds down the bottom
  set showmode                    " Show current mode down the bottom
  set gcr=a:blinkon0              " Disable cursor blink
  set visualbell                  " No sounds
  set autoread                    " Reload files changed outside vim
  set laststatus=2                " Always show the statusline
  set encoding=utf-8              " Necessary to show unicode glyphs

" This makes vim act like all other editors, buffers can
" exist in the background without being in a window.
" http://items.sjbach.com/319/configuring-vim-right

  set hidden

" Turn on syntax highlighting
  syntax on

" ================ Search Settings  =================

  set incsearch        " Find the next match as we type the search
  set hlsearch         " Hilight searches by default
  set viminfo='100,f1  " Save up to 100 marks, enable capital marks

  " Make /-style searches case-sensitive only if there is a capital letter in
  " the search expression. *-style searches continue to be consistently
  " case-sensitive.
  set ignorecase
  set smartcase

" ================ Turn Off Swap Files ==============

  set noswapfile
  set nobackup
  set nowb

" ================ Indentation ======================

  set autoindent
  set smartindent
  set smarttab
  set shiftwidth=2
  set softtabstop=2
  set tabstop=2
  set expandtab

  filetype plugin on
  filetype indent on

" Display tabs and trailing spaces visually
  set list listchars=tab:\ \ ,trail:·

" ================ Folds ============================

  set foldmethod=indent   " fold based on indent
  set foldnestmax=3       " deepest fold is 3 levels
  set nofoldenable        " dont fold by default

" ================ Scrolling ========================

  set scrolloff=8         " start scrolling when we're 8 lines away from margins
  set sidescrolloff=15

" ================ Appearance =======================
" Use dark Solarized color theme

  se t_Co=256
  set background=dark
  colorscheme solarized

" ================ Completion =======================

  set wildmode=list,longest
  set wildmenu                      " enable ctrl-n and ctrl-p to scroll thru matches
  set wildignore=*.o,*.obj,*.pyc,*~ " stuff to ignore when tab completing

" Misc
  set matchpairs+=<:>     " show matching <> (html mainly) as well

" Delete trailing white space and Dos-returns and to expand tabs to spaces.
"nnoremap S :set et<CR>:retab!<CR>:%s/[\r \t]\+$//<CR>


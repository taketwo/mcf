" General config {{{

  " Prepare Vim for fish shell
  if &shell =~# 'fish$'
    set shell=sh
  endif

  set nocompatible                " Use Vim settings, rather then Vi settings
  set number                      " Line numbers are good
  set relativenumber              " Relative line numbers are good
  set backspace=indent,eol,start  " Allow backspace in insert mode
  set history=1000                " Store lots of :cmdline history
  set showcmd                     " Show incomplete cmds down the bottom
  set showmode                    " Show current mode down the bottom
  set gcr=a:blinkon0              " Disable cursor blink
  set visualbell                  " No sounds
  set autoread                    " Reload files changed outside vim
  set laststatus=2                " Always show the statusline
  set encoding=utf-8              " Necessary to show unicode glyphs
  set autowrite                   " Automatically save before e.g. :make
  set hidden
  syntax on                       " Turn on syntax highlighting
  let mapleader=','               " Change leader to a comma
  let maplocalleader='\'          " Change local leader to a backslash

" }}}
" Background color {{{

  " Set background option based on the current background color in terminal.
  " This should be done before loading plugins as some of them may depend
  " on the setting (e.g. vim-indent-guides).

  source ~/.vim/plugin/mcf-gnome.vim

  if GetGnomeValue('background-color') == "'#00002B2B3636'"
    set background=dark
  else
    set background=light
  endif

" }}}
" Plugin initialization {{{

  source ~/.vim/plugins.vim       " Load all the bundles in ~/.vim/plugins.vim

" }}}
" Search settings {{{

  set incsearch                   " Find the next match as we type the search
  set hlsearch                    " Hilight searches by default
  set viminfo='100,f1             " Save up to 100 marks, enable capital marks

  " Make /-style searches case-sensitive only if there is a capital letter in
  " the search expression. *-style searches continue to be consistently
  " case-sensitive.
  set ignorecase
  set smartcase

" }}}
" Swap files {{{

  set noswapfile
  set nobackup
  set nowb

" }}}
" Indentation {{{

  set autoindent
  set smartindent
  set smarttab
  set shiftwidth=2
  set softtabstop=2
  set expandtab

  filetype plugin on
  filetype indent on

" }}}
" Folding {{{

  set foldmethod=indent   " Fold based on indent
  set foldnestmax=3       " Deepest fold is 3 levels
  set nofoldenable        " Don't fold by default

" }}}
" Scrolling {{{

  set scrolloff=8         " Start scrolling when we're 8 lines away from margins
  set sidescrolloff=15

" }}}
" Appearance {{{

  " Use Solarized color theme
  set t_Co=256

  colorscheme solarized

  hi SpellBad ctermfg=red            " Adjust the color of wrongly spelled words
  hi SignColumn ctermbg=bg           " Adjust the color of SignColumnt
  set list listchars=tab:→\ ,trail:· " Display tabs and trailing spaces visually

" }}}
" Completion {{{

  set wildmode=list,longest
  set wildmenu                         " Enable ctrl-n and ctrl-p to scroll thru matches
  set wildignore=*.o,*.obj,*.pyc,*~    " Stuff to ignore when tab completing

  set completeopt=menu,menuone,longest " Complete options (disable preview scratch window)
  set pumheight=15                     " Limit popup menu height

  set splitbelow                       " New splits, help, scratch preview all go to the bottom

" }}}
" Misc {{{

  set matchpairs+=<:>     " Show matching <> (html mainly) as well
  " %% expands to the full path of the directory that contains the current file
  cabbr <expr> %% expand('%:p:h')

" }}}

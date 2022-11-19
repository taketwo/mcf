" General config {{{

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
  set shortmess+=c                " Do not give ins-completion-menu messages
  let mapleader=','               " Change leader to a comma
  let maplocalleader='\'          " Change local leader to a backslash
  set lazyredraw                  " Do not redraw screen in the middle of a macro
  set updatetime=1000             " Down from default 4 seconds

" }}}
" Plugin initialization {{{

  let g:python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'

  " Load plugin settings that have to be set before loading plugins.
  " This should eventually be moved to packer config.
  for f in split(glob(stdpath('config') . '/settings/before/*.vim'), '\n')
    exe 'source' f
  endfor
  lua require('plugins')

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
  set inccommand=nosplit

" }}}
" Swap and undo files {{{

  set noswapfile
  set nobackup
  set nowb
  set undofile                    " Persistent undo is good

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

  " Use custom color theme that is based on Solarized light
  set termguicolors
  set background=light
  autocmd vimenter * ++nested colorscheme mcf

  set list listchars=tab:→\ ,trail:· " Display tabs and trailing spaces visually

" }}}
" Completion {{{

  set wildmode=longest:full
  set wildmenu                         " Enable ctrl-n and ctrl-p to scroll thru matches
  set wildignore=*.o,*.obj,*.pyc,*~    " Stuff to ignore when tab completing

  set completeopt=menuone,noselect     " Complete options (disable preview scratch window)
  set pumheight=15                     " Limit popup menu height

  set splitbelow                       " New splits, help, scratch preview all go to the bottom

" }}}
" Misc {{{

  set matchpairs+=<:>     " Show matching <> (html mainly) as well
  " %% expands to the full path of the directory that contains the current file
  cabbr <expr> %% expand('%:p:h')
  abbr ie i.e.
  abbr eg e.g.

" }}}

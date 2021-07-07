" Vim plugin configuration

" Load plugin settings. Some of them have to be set before
" loading plugins, otherwise they have no effect.
for f in split(glob('~/.vim/settings/before/*.vim'), '\n')
    exe 'source' f
endfor

" Plugin location depends on whether this in Vim or Neovim and is out of .vim/ tree
if has('nvim')
    let plugins_path = $HOME . '/.local/share/nvim/plugins'
else
    let plugins_path = $HOME . '/.local/share/vim/plugins'
endif

call plug#begin(plugins_path)

Plug 'andymass/vim-matchup'
Plug 'godlygeek/tabular'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeMirrorToggle' }
Plug 'jistr/vim-nerdtree-tabs', { 'on': 'NERDTreeMirrorToggle' }
Plug 'skwp/vim-colors-solarized'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'kshenoy/vim-signature'
Plug 'briandoll/change-inside-surroundings.vim'
Plug 'majutsushi/tagbar'
Plug 'craigemery/vim-autotag'
Plug 'tmsvg/pear-tree'
Plug 'scrooloose/nerdcommenter'
Plug 'w0rp/ale'
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-surround'
Plug 'embear/vim-localvimrc'
Plug 'qpkorr/vim-bufkill'
Plug 'AndrewRadev/sideways.vim'
Plug 'dantler/vim-alternate'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-dispatch'
Plug 'Peeja/vim-cdo'
Plug 'tpope/vim-abolish'
Plug 'mbbill/undotree'
Plug 'xolox/vim-misc'
Plug 'taketwo/vim-exchange'
Plug 'janko-m/vim-test'
Plug 'machakann/vim-highlightedyank'
Plug 'AndrewRadev/dsf.vim'
Plug 'tweekmonster/startuptime.vim'
Plug 'sickill/vim-pasta'
Plug 'taketwo/vimux'
Plug 'jeffkreeftmeijer/vim-numbertoggle'
Plug 'taketwo/diffchar.vim'
Plug 'AndrewRadev/deleft.vim'
Plug 'ryanoasis/vim-devicons'

" Navigation
Plug 'justinmk/vim-sneak'
Plug 'christoomey/vim-tmux-navigator'
Plug 'pechorin/any-jump.vim'

" Search
Plug 'henrik/vim-indexed-search'
Plug 'nelstrom/vim-visual-star-search'
Plug 'mileszs/ack.vim'
Plug 'dyng/ctrlsf.vim'
Plug 'beloglazov/vim-online-thesaurus'

" Languages / frameworks
" isort is (temporary) disabled because:
"   * do not use it that often
"   * does not work on the Arch box
" Plugin 'fisadev/vim-isort'

Plug 'plasticboy/vim-markdown'
Plug 'taketwo/vim-ros'
Plug 'tikhomirov/vim-glsl'
Plug 'digitaltoad/vim-pug'
Plug 'lervag/vimtex'
Plug 'keith/tmux.vim'
Plug 'LnL7/vim-nix'

" Git related
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'
Plug 'junegunn/gv.vim'
Plug 'whiteinge/diffconflicts'
Plug 'rhysd/git-messenger.vim'
Plug 'jreybert/vimagit'

" Text objects
Plug 'michaeljsmith/vim-indent-object'
Plug 'coderifous/textobj-word-column.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-function'
Plug 'jceb/vim-textobj-uri'
Plug 'machakann/vim-textobj-delimited'

" Snippets
Plug 'honza/vim-snippets'
Plug 'kkoomen/vim-doge'

" Misc
Plug 'liuchengxu/vim-which-key'
Plug 'voldikss/vim-floaterm'

" Non-vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all --no-update-rc' }
Plug 'junegunn/fzf.vim'

" Neovim-only plugins
if has('nvim')
    Plug 'kevinhwang91/rnvimr'
    Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
    Plug 'nvim-telescope/telescope.nvim' | Plug 'nvim-lua/popup.nvim' | Plug 'nvim-lua/plenary.nvim'
    Plug 'nvim-lua/completion-nvim'
    Plug 'neovim/nvim-lspconfig'
    Plug 'glepnir/lspsaga.nvim'
    Plug 'tversteeg/registers.nvim'
    Plug 'lukas-reineke/indent-blankline.nvim'
else
    Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM') }
    Plug 'terryma/vim-expand-region'   " superceded by treesitter in Neovim
    Plug 'Yggdroot/indentLine'         " superceded by indent-blankline in Neovim
endif

" Try these plugins:
"Plug 'sjl/splice.vim'
"Plug 'tpope/vim-repeat'
"Plug 'tpope/vim-unimpaired'
"Plug 'xolox/vim-notes'

call plug#end()

if !isdirectory(plugins_path)
    echo "Installing Vim plugins..."
    PlugClean
    PlugInstall
endif

" Vim plugin configuration

" Load plugin settings. Some of them have to be set before
" loading plugins, otherwise they have no effect.
for f in split(glob(stdpath('config') . '/settings/before/*.vim'), '\n')
    exe 'source' f
endfor

" Installed plugins go to the standard data location
let plugins_path = stdpath("data") . '/plugins'

call plug#begin(plugins_path)

Plug 'andymass/vim-matchup'
Plug 'godlygeek/tabular'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeMirrorToggle' }
Plug 'jistr/vim-nerdtree-tabs', { 'on': 'NERDTreeMirrorToggle' }
Plug 'lifepillar/vim-solarized8'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'kshenoy/vim-signature'
Plug 'briandoll/change-inside-surroundings.vim'
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
Plug 'dyng/ctrlsf.vim'

Plug 'preservim/vim-markdown'
Plug 'taketwo/vim-ros', { 'branch': 'cmp' }
Plug 'tikhomirov/vim-glsl'
Plug 'digitaltoad/vim-pug'
Plug 'lervag/vimtex'
Plug 'keith/tmux.vim'
Plug 'LnL7/vim-nix'
Plug 'hjson/vim-hjson'

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
Plug 'junegunn/fzf.vim'

" Misc unsorted
Plug 'kevinhwang91/rnvimr'
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
Plug 'nvim-treesitter/playground'
Plug 'nvim-telescope/telescope.nvim' | Plug 'nvim-lua/popup.nvim' | Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope-symbols.nvim'
Plug 'nvim-telescope/telescope-ui-select.nvim'
Plug 'nvim-telescope/telescope-file-browser.nvim'
Plug 'neovim/nvim-lspconfig'
Plug 'glepnir/lspsaga.nvim'
Plug 'tversteeg/registers.nvim'
Plug 'lukas-reineke/indent-blankline.nvim'
Plug 'onsails/lspkind-nvim'
Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer' 
Plug 'hrsh7th/cmp-omni'
Plug 'hrsh7th/cmp-path'
Plug 'quangnguyen30192/cmp-nvim-ultisnips'
Plug 'github/copilot.vim'
Plug 'folke/todo-comments.nvim'

call plug#end()

" Do not load configs of Lua-based plugins because this will fail if plugins
" are not installed.
if isdirectory(g:plugins_path)
    lua require('mcf.lsp')
    lua require('mcf.lspsaga')
    lua require('mcf.cmp')
    lua require('mcf.telescope')
    lua require('mcf.todo_comments')
    lua require('mcf.treesitter')
    lua require('mcf.registers')
    lua require('mcf.indent_blankline')
endif
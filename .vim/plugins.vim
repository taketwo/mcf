" Vim plugin configuration

" Load plugin settings. Some of them have to be set before
" loading plugins, otherwise they have no effect.
for f in split(glob('~/.vim/settings/before/*.vim'), '\n')
    exe 'source' f
endfor

call plug#begin('~/.vim/bundle')

Plug 'tmhedberg/matchit'
Plug 'godlygeek/tabular'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'henrik/vim-indexed-search'
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeMirrorToggle' }
Plug 'jistr/vim-nerdtree-tabs', { 'on': 'NERDTreeMirrorToggle' }
Plug 'justinmk/vim-sneak'
Plug 'skwp/vim-colors-solarized'
Plug 'vim-airline/vim-airline' | Plug 'vim-airline/vim-airline-themes'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'kshenoy/vim-signature'
Plug 'briandoll/change-inside-surroundings.vim'
Plug 'Yggdroot/indentLine'
Plug 'majutsushi/tagbar'
Plug 'craigemery/vim-autotag'
Plug 'Raimondi/delimitMate'
Plug 'scrooloose/nerdcommenter'
Plug 'w0rp/ale'
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-surround'
Plug 'embear/vim-localvimrc'
Plug 'qpkorr/vim-bufkill'
Plug 'szw/vim-g'
Plug 'AndrewRadev/sideways.vim'
Plug 'dantler/vim-alternate'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-dispatch'
Plug 'mileszs/ack.vim'
Plug 'Peeja/vim-cdo'
Plug 'terryma/vim-expand-region'
Plug 'tpope/vim-abolish'
Plug 'sjl/gundo.vim'
Plug 'nelstrom/vim-visual-star-search'
Plug 'Valloric/ListToggle'
Plug 'xolox/vim-misc'
Plug 'taketwo/vim-exchange'
Plug 'janko-m/vim-test'
Plug 'machakann/vim-highlightedyank'
Plug 'AndrewRadev/dsf.vim'
Plug 'beloglazov/vim-online-thesaurus'
Plug 'tweekmonster/startuptime.vim'
Plug 'sickill/vim-pasta'
Plug 'taketwo/vimux'
Plug 'christoomey/vim-tmux-navigator'
Plug 'dyng/ctrlsf.vim'

" Languages / frameworks
Plug 'Valloric/YouCompleteMe', { 'do': function('BuildYCM') }
"Plugin 'davidhalter/jedi-vim'
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
Plug 'gregsexton/gitv', {'on': ['Gitv']}
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'
Plug 'tpope/vim-rhubarb'
Plug 'airblade/vim-gitgutter'

" Text objects
Plug 'michaeljsmith/vim-indent-object'
Plug 'coderifous/textobj-word-column.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-function'
Plug 'jceb/vim-textobj-uri'
Plug 'lucapette/vim-textobj-underscore'

" Snippets
Plug 'honza/vim-snippets'

" Non-vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all && cd ../.mcf && git checkout -- .bashrc' }
Plug 'junegunn/fzf.vim'

" Try these plugins:
"Plug 'sjl/splice.vim'
"Plug 'tpope/vim-repeat'
"Plug 'tpope/vim-unimpaired'
"Plug 'xolox/vim-notes'

call plug#end()

let ycm_readme = expand('~/.vim/bundle/YouCompleteMe/README.md')
if !filereadable(ycm_readme)
    echo "Installing Vim plugins..."
    PlugClean
    PlugInstall
endif

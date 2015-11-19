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
Plug 'skwp/vim-easymotion'
Plug 'skwp/vim-colors-solarized'
Plug 'bling/vim-airline'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'tomtom/tlib_vim'
Plug 'kshenoy/vim-signature'
Plug 'briandoll/change-inside-surroundings.vim'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'kien/ctrlp.vim'
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'majutsushi/tagbar'
Plug 'vim-scripts/AutoTag'
Plug 'Raimondi/delimitMate'
Plug 'taketwo/nerdcommenter'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'tpope/vim-surround'
Plug 'embear/vim-localvimrc'
Plug 'fholgado/minibufexpl.vim'
Plug 'szw/vim-g'
Plug 'AndrewRadev/sideways.vim'
Plug 'dantler/vim-alternate'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-dispatch'
Plug 'taketwo/vim-ipython'
Plug 'rking/ag.vim'
Plug 'Peeja/vim-cdo'
Plug 'terryma/vim-expand-region'
Plug 'tpope/vim-abolish'
Plug 'sjl/gundo.vim'
Plug 'nelstrom/vim-visual-star-search'
Plug 'Valloric/ListToggle'
Plug 'xolox/vim-misc'
Plug 'rhysd/clever-f.vim'
Plug 'taketwo/vim-exchange'

" Languages / frameworks
Plug 'Valloric/YouCompleteMe', { 'do': './install.sh --clang-completer' }
"Plugin 'davidhalter/jedi-vim'
" isort is (temporary) disabled because:
"   * do not use it that often
"   * does not work on the Arch box
" Plugin 'fisadev/vim-isort'
Plug 'tpope/vim-markdown'
Plug 'nelstrom/vim-markdown-folding'
Plug 'taketwo/vim-ros'
Plug 'rhysd/vim-clang-format'
Plug 'tikhomirov/vim-glsl'

" Git related
Plug 'gregsexton/gitv'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-git'

" Text objects
Plug 'michaeljsmith/vim-indent-object'
Plug 'coderifous/textobj-word-column.vim'
Plug 'kana/vim-textobj-entire'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-function'
Plug 'lucapette/vim-textobj-underscore'

" Snippets
Plug 'honza/vim-snippets'

" Non-vim
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': 'yes \| ./install' }
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

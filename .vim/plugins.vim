" Vim plugin configuration
" Run this from command line to install/update plugins:
" vim --noplugin -u vim/vundles.vim -N "+set hidden" "+syntax on" +PluginClean! +PluginInstall +qall

" Setting up Vundle - the vim plugin bundler.
" Solution from: https://github.com/fisadev/fisa-vim-config
let just_installed_vundle = 0
let vundle_readme = expand('~/.vim/bundle/Vundle/README.md')
if !filereadable(vundle_readme)
    echo "Installing Vundle..."
    echo ""
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/Vundle ~/.vim/bundle/Vundle
    let just_installed_vundle = 1
endif

" Load plugin settings. Some of them have to be set before
" loading plugins, otherwise they have no effect.
for f in split(glob('~/.vim/settings/before/*.vim'), '\n')
    exe 'source' f
endfor

filetype off " Filetype off is required by vundle

set rtp+=~/.vim/bundle/Vundle/
call vundle#begin()

Plugin 'gmarik/Vundle'

Plugin 'tmhedberg/matchit'
Plugin 'godlygeek/tabular'
Plugin 'AndrewRadev/splitjoin.vim'
Plugin 'henrik/vim-indexed-search'
Plugin 'scrooloose/nerdtree'
Plugin 'jistr/vim-nerdtree-tabs'
Plugin 'skwp/vim-easymotion'
Plugin 'skwp/vim-colors-solarized'
Plugin 'bling/vim-airline'
Plugin 'MarcWeber/vim-addon-mw-utils'
Plugin 'tomtom/tlib_vim'
Plugin 'kshenoy/vim-signature'
Plugin 'briandoll/change-inside-surroundings.vim'
Plugin 'nathanaelkane/vim-indent-guides'
Plugin 'kien/ctrlp.vim'
Plugin 'FelikZ/ctrlp-py-matcher'
Plugin 'majutsushi/tagbar'
Plugin 'vim-scripts/AutoTag'
Plugin 'Raimondi/delimitMate'
Plugin 'taketwo/nerdcommenter'
Plugin 'scrooloose/syntastic'
Plugin 'SirVer/ultisnips'
Plugin 'tpope/vim-surround'
Plugin 'embear/vim-localvimrc'
Plugin 'fholgado/minibufexpl.vim'
Plugin 'szw/vim-g'
Plugin 'AndrewRadev/sideways.vim'
Plugin 'dantler/vim-alternate'
Plugin 'tpope/vim-speeddating'
Plugin 'tpope/vim-eunuch'
Plugin 'tpope/vim-dispatch'
Plugin 'taketwo/vim-ipython'
Plugin 'rking/ag.vim'
Plugin 'Peeja/vim-cdo'
Plugin 'terryma/vim-expand-region'
Plugin 'tpope/vim-abolish'
Plugin 'sjl/gundo.vim'
Plugin 'nelstrom/vim-visual-star-search'
Plugin 'Valloric/ListToggle'
Plugin 'xolox/vim-misc'
Plugin 'mhinz/vim-startify'
Plugin 'rhysd/clever-f.vim'
Plugin 'taketwo/vim-exchange'

" Languages / frameworks
Plugin 'Valloric/YouCompleteMe'
"Plugin 'davidhalter/jedi-vim'
" isort is (temporary) disabled because:
"   * do not use it that often
"   * does not work on the Arch box
" Plugin 'fisadev/vim-isort'
Plugin 'tpope/vim-markdown'
Plugin 'nelstrom/vim-markdown-folding'
Plugin 'smancill/conky-syntax.vim'
Plugin 'taketwo/vim-ros'
Plugin 'dag/vim-fish'
Plugin 'rhysd/vim-clang-format'

" Git related
Plugin 'gregsexton/gitv'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-git'

" Text objects
Plugin 'michaeljsmith/vim-indent-object'
Plugin 'coderifous/textobj-word-column.vim'
Plugin 'kana/vim-textobj-entire'
Plugin 'kana/vim-textobj-user'
Plugin 'kana/vim-textobj-function'
Plugin 'lucapette/vim-textobj-underscore'

" Snippets
Plugin 'honza/vim-snippets'

" Try these plugins:
"Plugin 'sjl/splice.vim'
"Plugin 'tpope/vim-repeat'
"Plugin 'tpope/vim-unimpaired'
"Plugin 'xolox/vim-notes'

call vundle#end()
filetype plugin indent on " Filetype plugin indent on is required by Vundle

if just_installed_vundle
    PluginClean!
    PluginInstall!
endif

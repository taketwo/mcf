" Vim plugin configuration
" Run this from command line to install/update plugins:
" vim --noplugin -u vim/vundles.vim -N "+set hidden" "+syntax on" +BundleClean! +BundleInstall +qall

" Setting up Vundle - the vim plugin bundler.
" Solution from: https://github.com/fisadev/fisa-vim-config
let just_installed_vundle = 0
let vundle_readme = expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
    echo "Installing Vundle..."
    echo ""
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
    let just_installed_vundle = 1
endif

" Load plugin settings. Some of them have to be set before
" loading plugins, otherwise they have no effect.
for f in split(glob('~/.vim/settings/before/*.vim'), '\n')
    exe 'source' f
endfor

filetype off " Filetype off is required by vundle

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

Bundle "gmarik/vundle"

Bundle "jeffkreeftmeijer/vim-numbertoggle"
Bundle "godlygeek/tabular"
Bundle "AndrewRadev/splitjoin.vim"
Bundle "maxbrunsfeld/vim-yankstack"
Bundle "henrik/vim-indexed-search"
Bundle "scrooloose/nerdtree"
Bundle "jistr/vim-nerdtree-tabs"
Bundle "skwp/vim-easymotion"
Bundle "skwp/vim-powerline"
Bundle "skwp/vim-colors-solarized"
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "xsunsmile/showmarks"
Bundle "briandoll/change-inside-surroundings.vim"
Bundle "nathanaelkane/vim-indent-guides"
Bundle "kien/ctrlp.vim"
Bundle "majutsushi/tagbar"
Bundle "vim-scripts/AutoTag"
Bundle "Raimondi/delimitMate"
Bundle "scrooloose/nerdcommenter"
Bundle "scrooloose/syntastic"
Bundle "ervandew/supertab"
Bundle "SirVer/ultisnips"
Bundle "kana/vim-arpeggio"
Bundle "tpope/vim-surround"
Bundle "embear/vim-localvimrc"
Bundle "fholgado/minibufexpl.vim"
Bundle "szw/vim-g"
Bundle "AndrewRadev/sideways.vim"
Bundle "dantler/vim-alternate"
Bundle "tpope/vim-speeddating"
Bundle "tpope/vim-eunuch"
Bundle "tpope/vim-dispatch"
Bundle "taketwo/vim-ipython"
Bundle "rking/ag.vim"
Bundle "Peeja/vim-cdo"
Bundle "terryma/vim-expand-region"
Bundle "terryma/vim-multiple-cursors"
Bundle "tpope/vim-abolish"
Bundle "sjl/gundo.vim"
Bundle "nelstrom/vim-visual-star-search"
Bundle "Valloric/ListToggle"

" Languages / frameworks
Bundle "Rip-Rip/clang_complete"
Bundle "davidhalter/jedi-vim"
Bundle "tpope/vim-markdown"
Bundle "nelstrom/vim-markdown-folding"
Bundle "smancill/conky-syntax.vim"
Bundle "taketwo/vim-ros"

" Git related
Bundle "gregsexton/gitv"
Bundle "tpope/vim-fugitive"
Bundle "tpope/vim-git"

" Text objects
Bundle "michaeljsmith/vim-indent-object"
Bundle "coderifous/textobj-word-column.vim"
Bundle "kana/vim-textobj-entire"
Bundle "kana/vim-textobj-user"
Bundle "kana/vim-textobj-function"
Bundle "lucapette/vim-textobj-underscore"

" Try these plugins:
"Bundle "sjl/splice.vim"
"Bundle "tpope/vim-repeat"
"Bundle "tpope/vim-unimpaired"

filetype plugin indent on " Filetype plugin indent on is required by Vundle

if just_installed_vundle
    BundleClean!
    BundleInstall!
endif

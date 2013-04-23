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
for f in split(glob('~/.vim/settings/*.vim'), '\n')
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
Bundle "tpope/vim-fugitive"
Bundle "skwp/vim-colors-solarized"
Bundle "MarcWeber/vim-addon-mw-utils"
Bundle "tomtom/tlib_vim"
Bundle "xsunsmile/showmarks"
Bundle "tpope/vim-git"
Bundle "briandoll/change-inside-surroundings.vim"
Bundle "gregsexton/gitv"
Bundle "nathanaelkane/vim-indent-guides"
Bundle "kien/ctrlp.vim"
Bundle "majutsushi/tagbar"
Bundle "vim-scripts/AutoTag"
Bundle "Raimondi/delimitMate"
Bundle "Rip-Rip/clang_complete"
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
Bundle "vim-scripts/bufkill.vim"
Bundle "tpope/vim-markdown"
Bundle "nelstrom/vim-markdown-folding"
Bundle "tpope/vim-speeddating"
Bundle "tpope/vim-eunuch"
Bundle "michaeljsmith/vim-indent-object"
Bundle "taketwo/vim-ipython"
Bundle "taketwo/vim-ros"
Bundle "rking/ag.vim"
Bundle "Peeja/vim-cdo"
Bundle "davidhalter/jedi-vim"
Bundle "terryma/vim-expand-region"

" Try these plugins:
"Bundle "sjl/splice.vim"
"Bundle "tpope/vim-repeat"
"Bundle "tpope/vim-unimpaired"
"Bundle "sjl/gundo.vim"
"Bundle "airblade/vim-gitgutter"

filetype plugin indent on " Filetype plugin indent on is required by Vundle

if just_installed_vundle
    BundleClean!
    BundleInstall!
endif

" Complete plugin setup. Some of the settings could be done only
" after a plugin has been loaded.
for f in split(glob('~/.vim/settings/after/*.vim'), '\n')
    exe 'source' f
endfor

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

"Bundle "tpope/vim-repeat.git"
"Bundle "tpope/vim-unimpaired"
"Bundle "sjl/gundo.vim"
Bundle "jeffkreeftmeijer/vim-numbertoggle.git"
Bundle "godlygeek/tabular"
Bundle "AndrewRadev/splitjoin.vim"
Bundle "skwp/YankRing.vim"
"Bundle "airblade/vim-gitgutter.git"
Bundle "henrik/vim-indexed-search.git"
Bundle "scrooloose/nerdtree.git"
Bundle "jistr/vim-nerdtree-tabs.git"
Bundle "skwp/vim-easymotion.git"
Bundle "skwp/vim-powerline.git"
Bundle "tpope/vim-fugitive.git"
Bundle "skwp/vim-colors-solarized.git"
Bundle "MarcWeber/vim-addon-mw-utils.git"
Bundle "tomtom/tlib_vim.git"
Bundle "xsunsmile/showmarks.git"
Bundle "tpope/vim-git.git"
Bundle "briandoll/change-inside-surroundings.vim.git"
Bundle "gregsexton/gitv.git"
Bundle "nathanaelkane/vim-indent-guides.git"
Bundle "vim-scripts/SearchComplete.git"
Bundle "kien/ctrlp.vim.git"
Bundle "majutsushi/tagbar.git"
Bundle "vim-scripts/AutoTag.git"
Bundle "Raimondi/delimitMate.git"
Bundle "Rip-Rip/clang_complete.git"
Bundle "scrooloose/nerdcommenter.git"
Bundle "scrooloose/syntastic.git"
Bundle "ervandew/supertab.git"
Bundle "SirVer/ultisnips.git"
Bundle "kana/vim-arpeggio.git"
Bundle "tpope/vim-surround.git"
Bundle "embear/vim-localvimrc.git"
Bundle "fholgado/minibufexpl.vim.git"
Bundle "szw/vim-g.git"
Bundle "AndrewRadev/sideways.vim.git"
Bundle "dantler/vim-alternate"
Bundle "vim-scripts/bufkill.vim.git"
Bundle "tpope/vim-markdown.git"
Bundle "nelstrom/vim-markdown-folding.git"
Bundle "tpope/vim-speeddating.git"
Bundle "tpope/vim-eunuch.git"
Bundle "michaeljsmith/vim-indent-object.git"
Bundle "tpope/vim-pathogen.git"
Bundle "taketwo/vim-ipython.git"
Bundle "taketwo/vim-ros.git"
Bundle "rking/ag.vim.git"
Bundle "Peeja/vim-cdo.git"

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

" Vim plugin configuration
" Run this from command line to install/update plugins:
" vim --noplugin -u vim/vundles.vim -N "+set hidden" "+syntax on" +BundleClean! +BundleInstall +qall

" Setting up Vundle - the vim plugin bundler.
" Solution from: https://github.com/fisadev/fisa-vim-config
let iCanHazVundle = 1
let vundle_readme = expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
    echo "Installing Vundle..."
    echo ""
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/gmarik/vundle ~/.vim/bundle/vundle
    let iCanHazVundle = 0
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

"Bundle "Raimondi/delimitMate"
"Bundle "vim-scripts/AnsiEsc.vim.git"
"Bundle "skwp/vim-ruby-conque"
"Bundle "kogakure/vim-sparkup.git"
"Bundle "tomtom/tcomment_vim.git"
"Bundle "nelstrom/vim-markdown-preview"
"Bundle "scrooloose/nerdtree.git"
"Bundle "jistr/vim-nerdtree-tabs.git"
"Bundle "vim-scripts/AutoTag.git"
"Bundle "vim-scripts/IndexedSearch"
"Bundle "scrooloose/syntastic.git"
"Bundle "sjbach/lusty.git"
"Bundle "tjennings/git-grep-vim"
"Bundle "skwp/greplace.vim"
"Bundle "tpope/vim-repeat.git"
"Bundle "tpope/vim-surround.git"
"Bundle "tpope/vim-rails.git"
"Bundle "vim-ruby/vim-ruby.git"
"Bundle "ecomba/vim-ruby-refactoring"
"Bundle "vim-scripts/matchit.zip.git"
"Bundle "tpope/vim-endwise.git"
"Bundle "skwp/vim-html-escape"
"Bundle "Shougo/neocomplcache.git"
"Bundle "skwp/vim-colors-solarized"
"Bundle "tpope/vim-fugitive"
"Bundle "skwp/vim-git-grep-rails-partial"
"Bundle "tpope/vim-unimpaired"
"Bundle "tpope/vim-git"
"Bundle "vim-scripts/lastpos.vim"
"Bundle "sjl/gundo.vim"
"Bundle "vim-scripts/sudo.vim"
"Bundle "mileszs/ack.vim"
"Bundle "nelstrom/vim-textobj-rubyblock"
"Bundle "kana/vim-textobj-user"
"Bundle "austintaylor/vim-indentobject"
"Bundle "kana/vim-textobj-datetime"
"Bundle "kana/vim-textobj-entire"
"Bundle "mattn/gist-vim"
"Bundle "godlygeek/tabular"
"Bundle "AndrewRadev/splitjoin.vim"
"Bundle "vim-scripts/argtextobj.vim"
"Bundle "bootleq/vim-textobj-rubysymbol"
"Bundle "nathanaelkane/vim-indent-guides"
"Bundle "tpope/vim-haml"
"Bundle "claco/jasmine.vim"
"Bundle "kana/vim-textobj-function"
"Bundle "kchmck/vim-coffee-script"
"Bundle "wavded/vim-stylus"
"Bundle "vim-scripts/Vim-R-plugin"
"Bundle "kien/ctrlp.vim"
"Bundle "majutsushi/tagbar.git"
"Bundle "chrisbra/color_highlight.git"
"Bundle "vim-scripts/camelcasemotion.git"
"Bundle "garbas/vim-snipmate.git"
"Bundle "MarcWeber/vim-addon-mw-utils.git"
"Bundle "rking/ag.vim.git"
"Bundle "tomtom/tlib_vim.git"
"Bundle "honza/vim-snippets.git"
"Bundle "skwp/vim-conque"
"Bundle "gregsexton/gitv"
"Bundle "briandoll/change-inside-surroundings.vim.git"
"Bundle "timcharper/textile.vim.git"
"Bundle "vim-scripts/Specky.git"
"Bundle "tpope/vim-bundler"
"Bundle "tpope/vim-rake.git"
"Bundle "skwp/vim-easymotion"
"Bundle "groenewege/vim-less.git"
"Bundle "mattn/webapi-vim.git"
"Bundle "astashov/vim-ruby-debugger"
"Bundle "aaronjensen/vim-sass-status.git"
"Bundle "skwp/vim-powerline.git"
"Bundle "briancollins/vim-jst"
"Bundle "pangloss/vim-javascript"
"Bundle "skwp/YankRing.vim"
"Bundle "tpope/vim-abolish"
"Bundle "jtratner/vim-flavored-markdown.git"
"Bundle "xsunsmile/showmarks.git"
"Bundle "digitaltoad/vim-jade.git"
"Bundle "tpope/vim-ragtag"
"Bundle "vim-scripts/TagHighlight.git"
"Bundle "itspriddle/vim-jquery.git"
"Bundle "slim-template/vim-slim.git"
"Bundle "airblade/vim-gitgutter.git"
"Bundle "bogado/file-line.git"
"Bundle "tpope/vim-rvm.git"
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
Bundle "skwp/greplace.vim.git"
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

" Complete plugin setup. Some of the settings could be done only
" after a plugin has been loaded.
for f in split(glob('~/.vim/settings/after/*.vim'), '\n')
    exe 'source' f
endfor

set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
let g:python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
set inccommand=nosplit
source ~/.vimrc

" Neovim displays wildmenu completion using vertical menu, use this feature
set wildmode=longest:full

lua require('lsp_config')
lua require('lspsaga_config')

highlight LspDiagnosticsDefaultError ctermfg=160
highlight LspDiagnosticsDefaultWarning ctermfg=166

highlight LspDiagnosticsSignError ctermfg=160 ctermbg=7
highlight LspDiagnosticsSignwarning ctermfg=166 ctermbg=7

highlight LspReferenceRead cterm=bold
highlight LspReferenceText cterm=bold
highlight LspReferenceWrite cterm=bold

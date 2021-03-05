set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
let g:python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
set inccommand=nosplit
source ~/.vimrc

" Neovim displays wildmenu completion using vertical menu, use this feature
set wildmode=longest:full

lua require('lsp_config')

highlight LspDiagnosticsSignError ctermfg=1 ctermbg=7

sign define LspDiagnosticsSignError text=✸ texthl=LspDiagnosticsSignError linehl= numhl=
sign define LspDiagnosticsSignWarning text=✶ texthl=LspDiagnosticsSignWarning linehl= numhl=
sign define LspDiagnosticsSignInformation text=I texthl=LspDiagnosticsSignInformation linehl= numhl=
sign define LspDiagnosticsSignHint text=H texthl=LspDiagnosticsSignHint linehl= numhl=

highlight LspReferenceRead cterm=bold
highlight LspReferenceText cterm=bold
highlight LspReferenceWrite cterm=bold

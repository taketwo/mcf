set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
let g:python3_host_prog = '$PYENV_ROOT/versions/py3nvim/bin/python'
set inccommand=nosplit
source ~/.vimrc

" Neovim displays wildmenu completion using vertical menu, use this feature
set wildmode=longest:full

" Do not load configs of Lua-based plugins because this will fail if plugins
" are not installed.
if isdirectory(g:plugins_path)
    lua require('mcf.lsp')
    lua require('mcf.lspsaga')
    lua require('mcf.cmp')
    lua require('mcf.telescope')
    lua require('mcf.treesitter')
    lua require('mcf.registers')
endif

highlight DiagnosticError ctermfg=160
highlight DiagnosticWarn ctermfg=166

highlight DiagnosticSignError ctermfg=160 ctermbg=7
highlight DiagnosticSignWarn ctermfg=166 ctermbg=7

highlight LspReferenceRead cterm=bold
highlight LspReferenceText cterm=bold
highlight LspReferenceWrite cterm=bold

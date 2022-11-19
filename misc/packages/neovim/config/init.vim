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
    lua require('mcf.todo_comments')
    lua require('mcf.treesitter')
    lua require('mcf.registers')
    lua require('mcf.indent_blankline')
endif

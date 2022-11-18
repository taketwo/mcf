" Because vim-glsl removed support for *.glsl extension
" https://github.com/tikhomirov/vim-glsl/commit/d25e0a2aa2ea6ff7b021907a45f1ca1e53040ce7
autocmd! BufNewFile,BufRead *.glsl set filetype=glsl

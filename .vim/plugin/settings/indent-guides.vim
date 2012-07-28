let g:indent_guides_auto_colors = 0
let g:indent_guides_start_level = 2
let g:indent_guides_guide_size = 2
let g:indent_guides_enable_on_vim_startup = 1
autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd ctermbg=bg
autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=black

" Guide size will be set equal to shiftwidth parameter
let g:indent_guides_guide_size = 0
let g:indent_guides_start_level = 2
let g:indent_guides_enable_on_vim_startup = 1

" We introduce custom backgrounds for odd and even guides
" which will vary depending on the background theme
let g:indent_guides_auto_colors = 0
if $BACKGROUND == "light"
  autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd ctermbg=bg
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=lightgrey
else
  autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd ctermbg=bg
  autocmd VimEnter,Colorscheme * :hi IndentGuidesEven ctermbg=black
endif

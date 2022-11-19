" Mirror the current line
" http://vim.wikia.com/wiki/Reverse_selected_text
command! -bar -range Mirror <line1>,<line2>call setline('.', join(reverse(split(getline('.'), '\zs')), ''))

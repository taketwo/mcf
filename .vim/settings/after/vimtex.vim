" Steal F3 from Tagbar
" Vimtex TOC serves the same purpose, but is more powerful
nnoremap <F3> :VimtexTocToggle<CR>

function! s:map(mode, lhs, rhs, ...)
if (a:0 > 0) || (maparg(a:lhs, a:mode) ==# '')
  silent execute a:mode . 'map <silent><buffer>' a:lhs a:rhs
endif
endfunction

" Motion commands
call s:map('n', ']]', '<plug>(vimtex-]])')
call s:map('n', '][', '<plug>(vimtex-][)')
call s:map('n', '[]', '<plug>(vimtex-[])')
call s:map('n', '[[', '<plug>(vimtex-[[)')
call s:map('x', ']]', '<plug>(vimtex-]])')
call s:map('x', '][', '<plug>(vimtex-][)')
call s:map('x', '[]', '<plug>(vimtex-[])')
call s:map('x', '[[', '<plug>(vimtex-[[)')
call s:map('o', ']]', '<plug>(vimtex-]])')
call s:map('o', '][', '<plug>(vimtex-][)')
call s:map('o', '[]', '<plug>(vimtex-[])')
call s:map('o', '[[', '<plug>(vimtex-[[)')
call s:map('n', '%', '<plug>(vimtex-%)', 1)
call s:map('x', '%', '<plug>(vimtex-%)', 1)
call s:map('o', '%', '<plug>(vimtex-%)', 1)

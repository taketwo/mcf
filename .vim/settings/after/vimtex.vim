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

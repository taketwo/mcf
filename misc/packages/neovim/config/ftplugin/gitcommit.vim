" Problem:
" When you open git status with Fugitive, it maps <C-P> and <C-N> to jump
" between modified files. I find it easier to re-use left/right motion
" keys for that. The problem though is that gitcommit filetype is used by
" both ":Gstatus" and plain "git commit" from command line. In the latter
" case left/right motion keys are still needed for their original purpose,
" so I want to avoid re-mapping in that case.
"
" Solution:
" Check if this gitcommit file was opened by Fugitive by figuring out if
" there exists a custom mapping for "C". When this is the case we do not
" need left/right motion and the keys are re-used for forward/backward
" jumps. Otherwise the original left/right motion behavior is preserved.

nnoremap <buffer> <silent> n :call <SID>RightOrNext()<CR>
nnoremap <buffer> <silent> h :call <SID>LeftOrPrevious()<CR>

function! s:StartedFromFugitive()
    return maparg("cc") == ":<C-U>Gcommit<CR>"
endfunction

function! s:RightOrNext()
    if s:StartedFromFugitive()
        execute "normal \<C-N>"
    else
        normal! l
    endif
endfunction

function! s:LeftOrPrevious()
    if s:StartedFromFugitive()
        execute "normal \<C-P>"
    else
        normal! h
    endif
endfunction

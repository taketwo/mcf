fun! PackageComplete(findstart, base)
  if a:findstart
    " locate the start of the word
    let line = getline('.')
    let start = col('.') - 1
    while start > 0 && line[start - 1] != '"'
      let start -= 1
    endwhile
    return start
  else
    " find packages matching with "a:base"
    let res = []
    for m in split(system('rospack list-names'), "\n")
      if m =~ '^' . a:base
        call add(res, m)
      endif
    endfor
    return res
  endif
endfun


" For now this seems to be the only way to promote my custom
" function to the user auto-complete. Both commented-out
" methods below do not work for some reason.
au BufEnter * setlocal completefunc=PackageComplete

"call SuperTabSetDefaultCompletionType("<c-x><c-u>")
"setlocal completefunc=PackageComplete

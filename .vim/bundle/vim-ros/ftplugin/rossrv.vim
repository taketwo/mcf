if exists("b:did_ftplugin")
  finish
endif

function! rossrv#Complete(findstart, base)
  if a:findstart
    return 0
  else
    let res = []
    for m in split(system('rosmsg list'), "\n")
      if m =~ '^' . a:base
        call add(res, m)
      endif
    endfor
    return res
  endif
endfun

setlocal omnifunc=rossrv#Complete
"au BufEnter * setlocal omnifunc=rosmsg#Complete

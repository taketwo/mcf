au BufRead *.cpp call s:isGoogleTest()

function! s:isGoogleTest()
  let n = 1
  while n < 50
    let line = getline(n)
    let n = n + 1
    if line =~ '^#include <gtest/gtest.h>$'
      setlocal ft=gtest.cpp
      return
    endif
  endwhile
endfunction

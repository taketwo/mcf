" Automatically give executable permissions if file begins with #! and contains
" '/bin/' in the path

function! ChmodUX()
  if getline(1) =~ "^#!"
    if getline(1) =~ "/bin/"
      silent !chmod u+x <afile>
    endif
  endif
endfunction

au bufwritepost * :call ChmodUX()

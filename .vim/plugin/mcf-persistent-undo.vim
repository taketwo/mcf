if &undodir == "."
    set undodir=~/.vim/undo
endif

if !isdirectory(&undodir)
    call mkdir(&undodir, "p")
endif

set undofile

" Delete undo files that are older than 90 days
let s:undos = split(globpath(&undodir, "*"), "\n")
call filter(s:undos, "getftime(v:val) < localtime() - (60 * 60 * 24 * 90)")
call map(s:undos, "delete(v:val)")

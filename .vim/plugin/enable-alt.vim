" This script will enable Alt+'letter' combinations
" if g:enable_alt is set to 1
if exists("g:enable_alt") && g:enable_alt == 1
  let c='a'
  while c <= 'z'
    exec "set <A-".c.">=\e".c
    exec "imap \e".c." <A-".c.">"
    let c = nr2char(1+char2nr(c))
  endw
  set timeout ttimeoutlen=50
endif

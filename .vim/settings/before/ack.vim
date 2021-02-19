if executable('rg')
  let g:ackprg = 'rg --vimgrep --smart-case'
elseif executable('ag')
  let g:ackprg = 'ag --vimgrep --smart-case'
endif

" Highlight the searched term
let g:ackhighlight = 1

" Claim back 't' so that we can navigate between quickfix items with 'c'/'t'
" Jump to the next item in the list with Tab
let g:ack_mappings = {
\   't' : 'j',
\   '<Tab>' : ':cn<CR>'
\ }


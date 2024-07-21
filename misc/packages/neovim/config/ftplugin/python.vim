let maplocalleader = '\'

setlocal colorcolumn=88
setlocal nosmartindent  " TODO: Figure out whether this is a good idea
autocmd InsertLeave * if pumvisible() == 0 | pclose | endif

" Insert PUDB breakpoint after current line
function! InsertBreakpoint()
    call feedkeys("opudb\<C-R>=UltiSnips#ExpandSnippet()\<CR>", "x")
endfunction

let b:match_words = '\<if\>:\<elif\>:\<else\>'
let b:match_skip = 'R:^\s*'

lua << EOF
require('nvim-surround').buffer_setup({
  surrounds = {
    ['e'] = { add = { 'enumerate(', ')' } },
    ['l'] = { add = { 'len(', ')' } },
    ['r'] = { add = { 'range(', ')' } },
  },
})

require('which-key').add({
   buffer = 0,
   { '<LocalLeader>b', '<cmd>call InsertBreakpoint()<CR>', desc = 'Insert PuDB breakpoint' },
   { '<LocalLeader>:', '<cmd>call AppendColon()<CR>', desc = 'Append colon' },
})
EOF

setlocal textwidth=79
setlocal colorcolumn=80
setlocal foldmethod=marker

nnoremap <buffer> <Space><Space> gq}

abbr ie \ie
abbr eg \eg

call textobj#user#plugin('latex', {
\  'math-a': {
\     'pattern': '[$][^$]*[$]',
\     'select': 'am',
\   },
\  'math-i': {
\     'pattern': '[$]\zs[^$]*\ze[$]',
\     'select': 'im',
\   }
\ })

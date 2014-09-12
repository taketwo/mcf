" Do not use S-Tab
let g:ycm_key_list_previous_completion = ['<Up>']
" Do not use <Leader>d
let g:ycm_key_detailed_diagnostics = '<Leader>D'

let g:ycm_semantic_triggers = {
\   'roslaunch' : ['="', '$(', '/'],
\   'rosmsg,rossrv,rosaction' : ['re!^'],
\ }

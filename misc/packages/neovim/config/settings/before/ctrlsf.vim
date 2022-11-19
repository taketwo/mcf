let g:ctrlsf_auto_focus = {
    \ "at": "start"
    \ }

let g:ctrlsf_mapping = {
    \ "open" : "O",
    \ "openb": { "key": "o", "suffix": "<C-w>p" },
    \ "split": "",
    \ "vsplit": "",
    \ "tab": "",
    \ "tabb": "",
    \ "popen": "p",
    \ "popenf": "P",
    \ "quit": "q",
    \ "next": ">",
    \ "prev": "<",
    \ "pquit": "",
    \ "loclist": "",
    \ "stop": "",
    \ }

nmap <C-f> <Plug>CtrlSFPrompt
vmap <C-f> <Plug>CtrlSFVwordExec

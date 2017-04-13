" Do not use S-Tab
let g:ycm_key_list_previous_completion = ['<Up>']
" Do not use <Leader>d
let g:ycm_key_detailed_diagnostics = '<Leader>D'

let g:ycm_global_ycm_extra_conf = $MCF.'/misc/.ycm_extra_conf.py'

function! BuildYCM(info)
    if a:info.status == 'installed' || a:info.status == 'updated' || a:info.force
        let opts = ' --clang-completer'
        if tlib#sys#IsExecutable('npm')
            let opts = opts.' --tern-completer'
        endif
        execute '!./install.py'.opts
    endif
endfunction

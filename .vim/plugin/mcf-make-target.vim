" Add a custom command "MakeTarget" that accepts an optional argument,
" the name of the target to make. The name is stored and will be used
" in subsequently when "MakeTarget" is invoked without arguments.

if !exists('g:mcf_make_target')
    let g:mcf_make_target = ""
endif

function! MakeTarget(...)
    if a:0 > 0
        let g:mcf_make_target = a:1
    endif
    execute "Make ".g:mcf_make_target
endfunction

function! s:MakeTargetComplete(ArgLead, CmdLine, CursorPos)
    let targets = system(&makeprg." -qp | awk -F':' '/^[a-zA-Z0-9][^$#\\/\\t=]*:([^=]|$)/ {split($1,A,/ /);for(i in A)print A[i]}'")
    return filter(sort(split(targets, '\n')), 'stridx(v:val, a:ArgLead) != -1')
endfunction

command! -nargs=* -complete=customlist,s:MakeTargetComplete MakeTarget :call MakeTarget(<f-args>)

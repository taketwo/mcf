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

command! -nargs=* MakeTarget :call MakeTarget(<f-args>)

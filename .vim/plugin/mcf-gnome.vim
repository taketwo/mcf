" Get the name of Gnome profile
function! <SID>GetGnomeProfile()
    let shellcmd = 'dconf list /org/gnome/terminal/legacy/profiles:/'
    return substitute(system(shellcmd), "[\n]", "", "g")
endfunction

" Return 0 if Gnome tools needed for querying values are not available
" Return 1 otherwise
function! <SID>CheckGnomeTools()
    return executable('dconf')
endfunction

" Get the value of a Gnome value
" Returns empty string if the tools needed to query the value are not available
function! GetGnomeValue(name)
    if <SID>CheckGnomeTools()
        let profile = <SID>GetGnomeProfile()
        let name = substitute(a:name, "[_]", "-", "g")
        let shellcmd = 'dconf read /org/gnome/terminal/legacy/profiles:/' . profile . name
        let value = substitute(system(shellcmd), "[\n]", "", "g")
        return substitute(value, "^'\\(.*\\)'$", "\\1", "")
    else
        return ""
    endif
endfunction

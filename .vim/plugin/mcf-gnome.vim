" Return 0 if Gnome version is below 3.8
" Return 1 otherwise
function! <SID>CheckGnomeVersion()
    let shellcmd = 'gnome-terminal --version | grep -Po "(?<=Terminal )[0-9\.]*"'
    let vstr = substitute(system(shellcmd), "[\n]", "", "g")
    let vlist = split(vstr, '\.')
    let major = str2nr(vlist[0])
    let minor = str2nr(vlist[1])
    if (major == 3 && minor >= 8) || major >= 4
        return 1
    else
        return 0
    endif
endfunction

" Get the name of Gnome profile
function! <SID>GetGnomeProfile()
    if <SID>CheckGnomeVersion()
        let shellcmd = 'dconf read /org/gnome/terminal/legacy/profiles:/default'
    else
        let shellcmd = 'gconftool-2 -R /apps/gnome-terminal/profiles | grep /apps/gnome-terminal/profiles | cut -d/ -f5 | cut -d: -f1'
    endif
    return substitute(system(shellcmd), "[\n]", "", "g")
endfunction

" Get the value of a Gnome value
function! GetGnomeValue(name)
    let profile = <SID>GetGnomeProfile()
    if <SID>CheckGnomeVersion()
        let name = substitute(a:name, "[_]", "-", "g")
        let shellcmd = 'dconf read /org/gnome/terminal/legacy/profiles:/:' . profile . '/' . name
    else
        let name = substitute(a:name, "[-]", "_", "g")
        let shellcmd = 'gconftool-2 --get "/apps/gnome-terminal/profiles/' . profile . '/' . name . '"'
    endif
    let value = substitute(system(shellcmd), "[\n]", "", "g")
    return substitute(value, "^'\\(.*\\)'$", "\\1", "")
endfunction

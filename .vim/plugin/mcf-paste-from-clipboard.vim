" Enable paste mode, paste the contents of the system clipboard
function! PasteFromClipboard()
    set paste
    !xdotool key ctrl+shift+v
    !xdotool key Escape
    !xdotool key shift+F5
endfunction

export PATH="${PATH}:~/.mcf/scripts/bin"
xkbcomp $MCF/xkb $DISPLAY
xcompmgr &
unclutter -grab &
xsetroot -cursor_name left_ptr

nitrogen --restore

exec cinnamon-session --session=xmonad

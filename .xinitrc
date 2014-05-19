export PATH="${PATH}:~/.mcf/scripts/bin"
xkbcomp $MCF/xkb $DISPLAY
xcompmgr &
unclutter -grab &
xsetroot -cursor_name left_ptr

if hash xcape 2>/dev/null; then
  xcape -e 'Alt_L=Escape;Alt_R=Return;Control_L=Escape;Super_L=Super_L|0'
fi

exec cinnamon-session --session=xmonad

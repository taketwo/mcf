if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

export PATH="${PATH}:~/.mcf/scripts/bin"
xkbcomp $MCF/xkb $DISPLAY
xcompmgr &
unclutter -grab &
xsetroot -cursor_name left_ptr

nitrogen --restore

if hash cinnamon-session 2>/dev/null; then
  exec cinnamon-session --session=xmonad
else
  exec gnome-session --session=xmonad
fi

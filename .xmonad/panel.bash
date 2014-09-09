StartIndicator ()
{
  eval "__output=' '"
}

Fg ()
{
  if [ "$__output" != " " ] ; then
    eval "__output='^fg($1)$__output^fg()'"
  fi
}

Bg ()
{
  if [ "$__output" != " " ] ; then
    eval "__output='^bg($1)$__output^bg()'"
  fi
}

# Surround the segment with a clickable area
# Arguments:
#   1 Mouse button
#   2 Action
# Example:
#   Click 1 "keyboard -n"
Click ()
{
  if [ "$__output" != " " ] ; then
    eval "__output='^ca($1, $2)$__output^ca()'"
  fi
}

Icon ()
{
  eval "__output='$__output^i(/home/sergey/.xmonad/icons/${1}.xbm) '"
}

Add ()
{
  # Escape single quote with triple hat
  eval "__output='$__output${1//\'/^^^} '"
}

FlushIndicator ()
{
  if [ "$__output" != " " ] ; then
    # Undo triple hat escape
    echo -n "${__output//^^^/\'}"
  fi
}

NewLine ()
{
  eval '__popup=("${__popup[@]}" "$__output")'
  eval '__output=""'
}

StartPopup ()
{
  eval "__output=''"
  eval "__popup=('')"
}

FlushPopup ()
{
  if [ "$__popup" != " " ] ; then
    width=480
    font_pixelsize=11
    line_height=$((font_pixelsize + 4))
    lines=${#__popup[@]}
    height=$((line_height * (lines + 1)))
    bottom_gap=20
    read _ s_width <<< "$(xwininfo -root | egrep Width)"
    read _ s_height <<< "$(xwininfo -name "dzen title" | egrep Height)"
   {
      for line in "${__popup[@]}"; do
        # Undo triple hat escape
        echo "${line//^^^/\'}"
      done
      echo '^uncollapse()'
   } | dzen2 -x $((s_width - width)) -y $s_height -w $width -l $lines -h $line_height -sa left -fn "Liberation Mono:pixelsize=$font_pixelsize" -e 'leaveslave=exit;button1=exit;button3=exit;onstart=uncollapse' -p -bg "#3c3b37" -title-name 'gmail_popup'
  fi
}


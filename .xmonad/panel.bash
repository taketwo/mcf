solarizedGreen="#859900"
solarizedRed="#dc322f"
solarizedBlue="#268bd2"
solarizedCyan="#2aa198"
solarizedYellow="#b58900"
solarizedBase3="#fdf6e3"

Start ()
{
  eval "output=' '"
}

Fg ()
{
  if [ "$output" != " " ] ; then
    eval "output='^fg($1)$output^fg()'"
  fi
}

Bg ()
{
  if [ "$output" != " " ] ; then
    eval "output='^bg($1)$output^bg()'"
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
  if [ "$output" != " " ] ; then
    eval "output='^ca($1, $2)$output^ca()'"
  fi
}

Icon ()
{
  eval "output='$output^i(/home/sergey/.xmonad/icons/${1}.xbm) '"
}

Add ()
{
  # Escape single quote with triple hat
  eval "output='$output${1//\'/^^^} '"
}

Flush ()
{
  if [ "$output" != " " ] ; then
    # Undo triple hat escape
    echo -n "${output//^^^/\'}"
  fi
}

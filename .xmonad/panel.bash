SolarizedBase03="#002b36"
SolarizedBase02="#073642"
SolarizedBase01="#586e75"
SolarizedBase00="#657b83"
SolarizedBase0="#839496"
SolarizedBase1="#93a1a1"
SolarizedBase2="#eee8d5"
SolarizedBase3="#fdf6e3"
SolarizedYellow="#b58900"
SolarizedOrange="#cb4b16"
SolarizedRed="#dc322f"
SolarizedMagenta="#d33682"
SolarizedViolet="#6c71c4"
SolarizedBlue="#268bd2"
SolarizedCyan="#2aa198"
SolarizedGreen="#859900"

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

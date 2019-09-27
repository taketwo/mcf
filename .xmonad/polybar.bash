# Collection of bash helper functions to create segments for Polybar panel

StartSegment ()
{
  eval "__output=' '"
}

# Surround the segment with color tags
# Arguments:
#   1 Foreground color
#   2 Background color (optional)
# Example:
#   Color "#ff00ee" $SolarizedCyan
Color ()
{
  if [ "$__output" != " " ] ; then
    if [[ $# -gt 1 ]] ; then
      eval "__output='%{B$2 F$1}$__output%{B- F-}'"
    else
      eval "__output='%{F$1}$__output%{F-}'"
    fi
  fi
}

# Surround the segment with a clickable area
# Arguments:
#   1 Mouse button
#   2 Command
# Example:
#   Action 1 "keyboard -n"
Action ()
{
  if [ "$__output" != " " ] ; then
    eval "__output='%{A${1}:${2}:}$__output%{A}'"
  fi
}

Add ()
{
  # Escape single quote with triple hat
  eval "__output='$__output${1//\'/^^^} '"
}

FlushSegment ()
{
  if [ "$__output" != " " ] ; then
    # Undo triple hat escape
    echo -n "${__output//^^^/\'}"
  else
    echo ""
  fi
}

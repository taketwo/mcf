# -----------------------------------
#      commands
# -----------------------------------
#   0  clear
#   0  reset
#   1  ansibold     22  noansibold
#   3  italic       23  noitalic
#   4  underscore   24  nounderscore
#   5  blink        25  noblink
#   7  inverse      27  noinverse
#   8  concealed    28  noconcealed
# -----------------------------------
#      foreground       background
# -----------------------------------
#  30  black        40  on_black
#  31  red          41  on_red
#  32  green        42  on_green
#  33  yellow       43  on_yellow
#  34  blue         44  on_blue
#  35  magenta      45  on_magenta
#  36  cyan         46  on_cyan
#  37  white        47  on_white
# ------------------------------------
#      style
# ------------------------------------
#  mb  begin blinking
#  md  begin bold
#  me  end mode
#  se  end standout-mode
#  so  begin standout-mode - info box
#  ue  end underline
#  us  begin underline
# -----------------------------------

function man ()
{
  env LESS_TERMCAP_mb=$'\E[01;31m' \
  LESS_TERMCAP_md=$'\E[01;38;5;74m' \
  LESS_TERMCAP_me=$'\E[0m' \
  LESS_TERMCAP_se=$'\E[0m' \
  LESS_TERMCAP_so=$'\E[32;45;37m' \
  LESS_TERMCAP_ue=$'\E[0m' \
  LESS_TERMCAP_us=$'\E[04;38;5;125m' \
  man "$@"
}

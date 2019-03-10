function cxx ()
{
  if [[ $# -eq 0 ]]; then
    __cxx_status
  else
    case $1 in
      clang) __cxx_set "CC" "/usr/bin/clang" "$2"
             __cxx_set "CXX" "/usr/bin/clang++" "$2"
             ;;
        gcc) __cxx_set "CC" "/usr/bin/gcc" "$2"
             __cxx_set "CXX" "/usr/bin/g++" "$2"
             ;;
      reset) __cxx_reset
             ;;
    esac
  fi
}

# Print currently set compiler
function __cxx_status ()
{
  if [[ -z ${CXX+x} ]]; then
    echo "CXX: not set"
  else
    echo "CXX: $CXX"
  fi
  if [[ -z ${CC+x} ]]; then
    echo " CC: not set"
  else
    echo " CC: $CC"
  fi
}

# Set a default compiler in the current shell
function __cxx_set ()
{
  if [[ $3 -ne "" ]]; then
    bin="$2-$3"
  else
    bin="$2"
  fi
  if [[ ! -f $bin ]]; then
    echo "$bin does not exist"
  else
    eval "export $1='$bin'"
  fi
}

# Unset default compiler in current shell
function __cxx_reset ()
{
  unset CC
  unset CXX
}

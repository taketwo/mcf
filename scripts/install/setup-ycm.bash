#!/bin/bash

YCM="$MCF/.vim/bundle/YouCompleteMe"

# Test if YCM directory exists
if [[ ! -d $YCM ]]; then
  echo "YouCompleteMe is not installed"
  exit 1
fi

# Test if clang is available
if ! hash clang 2>/dev/null; then
  echo "clang is not installed"
  exit 2
fi

TEMP_DIR=`mktemp -d`

trap "{ cd - ; rm -rf $TEMP_DIR; exit 255; }" SIGINT

cd $TEMP_DIR

cmake -G "Unix Makefiles" -DUSE_SYSTEM_LIBCLANG=ON . $YCM/third_party/ycmd/cpp
make ycm_support_libs

cd -

rm -rf $TEMP_DIR

exit 0

if [[ -n $CLANG_VERSION ]]; then
  suffix="-${CLANG_VERSION}"
else
  suffix=""
fi

export CC="/usr/bin/clang${suffix}"
export CXX="/usr/bin/clang++${suffix}"

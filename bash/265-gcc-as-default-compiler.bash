if [[ -n $GCC_VERSION ]]; then
  suffix="-${GCC_VERSION}"
else
  suffix=""
fi

export CC="/usr/bin/gcc${suffix}"
export CXX="/usr/bin/g++${suffix}"

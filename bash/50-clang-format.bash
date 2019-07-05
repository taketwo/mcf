# If clang-format (without version suffix) is not available, create an alias
# that points to the highest available version.
if ! hash clang-format 2>/dev/null; then
  declare -a suffixes=("9.0" "8.0" "7.0" "6.0" "5.0" "4.0" "3.9" "3.8" "3.7" "3.6" "3.5")
  for suffix in "${suffixes[@]}"; do
    if hash "clang-format-$suffix" 2>/dev/null; then
      # shellcheck disable=SC2139
      alias clang-format="clang-format-$suffix"
      break
    fi
  done
fi

declare -a suffixes=("-5.0" "-4.0" "-3.9" "-3.8" "-3.7" "-3.6" "-3.5" "")
for suffix in "${suffixes[@]}"; do
  if hash "clang-format${suffix}" 2>/dev/null; then
    alias clang-format="clang-format${suffix}"
    break
  fi
done

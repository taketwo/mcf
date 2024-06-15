return {
  cmd = {
    'clangd',
    '--background-index',
    '--clang-tidy',
    '--completion-style=bundled', -- TODO: Check if detailed style is better
    '--function-arg-placeholders', -- TODO: Understand whether this is useful
  },
  filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'gtest.cpp' },
  capabilities = {
    offsetEncoding = 'utf-16',
  },
  -- Some of the supported options are listed at: https://clangd.llvm.org/extensions
  init_options = {
    usePlaceholders = true, -- TODO: Understand whether this is useful
    completeUnimported = true, -- TODO: Understand whether this is useful
  },
}

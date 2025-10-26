return {
  cmd = {
    'clangd',
    '--background-index',
    '--clang-tidy',
    '--completion-style=detailed',
  },
  filetypes = { 'c', 'cpp', 'objc', 'objcpp', 'gtest.cpp' },
  capabilities = {
    offsetEncoding = 'utf-16',
  },
  -- Some of the supported options are listed at: https://clangd.llvm.org/extensions
  -- Note that these are LSP options, not clangd options. The latter are configured
  -- in the global clangd configuration file.
  init_options = {
    usePlaceholders = true, -- TODO: Understand whether this is useful
    completeUnimported = true, -- TODO: Understand whether this is useful
  },
}
